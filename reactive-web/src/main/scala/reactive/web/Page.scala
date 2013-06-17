package reactive
package web

import scala.xml.NodeSeq
import scala.xml.{ Elem, Null, UnprefixedAttribute }

import net.liftweb.util.Helpers.randomString
import net.liftweb.json.JsonAST.JValue

/**
 * A Page uniquely identifies a web page rendered with reactive-web components.
 * It is used to associate RElems and ReactionsComets.
 * An RElem can be associated with multiple Pages. The corresponding
 * element will be kept in sync in both places.
 */
trait Page extends Logger {
  case class QueueingJS[T: CanRender](pageId: Option[String], transport: Transport, data: T) {
    def js: String = implicitly[CanRender[T]] apply data
  }

  /**
   * Use if you need to tie a listener's lifespan to the lifetime of the Page
   */
  implicit object observing extends Observing

  val id = randomString(20)

  private val counter = new java.util.concurrent.atomic.AtomicInteger(0)

  def nextId = "reactiveWebId_%06d" format nextNumber

  def nextNumber = counter.getAndIncrement

  private[web] val ajaxEvents = new EventSource[(String, JValue)] {
    override def debugName = Page.this.toString + ".ajaxEvents"
  }

  private val transports = new AtomicRef(List.empty[Transport])

  def linkTransport(t: Transport) = {
    t addPage this
    transports.transform(t +: _)
  }

  def unlinkTransport(t: Transport) = {
    transports.transform { _.filter(t ne _) }
    t removePage this
  }

  def withTransport[A](t: Transport)(f: =>A): A = {
    linkTransport(t)
    try f
    finally unlinkTransport(t)
  }

  /**
   * Queues javascript to be rendered via the available `Transport` with the highest priority
   */
  def queue[T: CanRender](renderable: T) =
    if(transports.get.isEmpty) sys.error("Page has no `Transport` installed")
    else {
      val preferredTransport = transports.get.maxBy(_.currentPriority)
      trace(QueueingJS(Some(id), preferredTransport, renderable))
      // println("Queuing: "+renderable + " (of class "+renderable.getClass+")")
      // println("Chose transport: " + preferredTransport)
      preferredTransport.queue(renderable)
    }

  def render: NodeSeq = NodeSeq.Empty
}

/**
 * Manages a WeakHashMap of Pages to ids, so for instance
 * an RElem may render under a different id for different Pages,
 * and it can record those ids, until their Page is garbage collected.
 */
trait PageIds extends Logger {
  case class AssignedNewId(pageId: String, id: String)

  protected var pageIds = new scala.collection.mutable.WeakHashMap[Page, String]()

  private def nextId(implicit page: Page) = {
    val ret = page.nextId
    trace(AssignedNewId(page.id, ret))
    ret
  }

  /**
   * Passes an Elem through this PageIds, either recording its id
   * for the current Page, or if it doesn't have the attribute,
   * add one from either the existing id for the Page, or if none exists,
   * generate a new one, add it to the Elem's attributes, and store it
   * in the WeakHashMap.
   * This method is called automatically, typically when rendering this object,
   * and is expected to be called once per Page.
   * Subclasses can add more new-Page-registering logic.
   */
  protected def addPage(elem: Elem)(implicit page: Page): Elem = synchronized {
    lazy val elemId = elem.attributes.get("id").map(_.text)
    val id = pageIds.getOrElseUpdate(page, elemId getOrElse nextId)
    elem % new UnprefixedAttribute("id", id, Null)
  }

  /**
   * The value of the id attribute for the Page.
   * If this is called before we have an id for the Page
   * one will be generated now, and that id will replace
   * the Elem's id in addPage if it had one.
   * On the other hand if addPage is first called with an
   * Elem that has an id, that will be returned and
   * no id will be generated.
   */
  def id(implicit page: Page): String = synchronized {
    pageIds.getOrElseUpdate(page, nextId)
  }
}
