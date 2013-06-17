package reactive
package web

import scala.xml.NodeSeq

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
    if(transports.get.isEmpty) sys.error("Page has no Transport installed")
    else {
      val preferredTransport = transports.get.maxBy(_.currentPriority)
      trace(QueueingJS(Some(id), preferredTransport, renderable))
      preferredTransport.queue(renderable)
    }

  def render: NodeSeq = NodeSeq.Empty
}
