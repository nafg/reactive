package reactive
package web

import scala.xml.NodeSeq

import net.liftweb.util.Helpers.randomString
import net.liftweb.json.JsonAST.JValue

import reactive.logging.Logger

trait IdCounter {
  protected val counter = new java.util.concurrent.atomic.AtomicInteger(0)

  def nextNumber = counter.getAndIncrement
}

trait PageComponent {
  private[web] val ajaxEvents = new EventSource[(String, JValue)] {
    override def debugName = PageComponent.this.toString + ".ajaxEvents"
  }

  private[web] val transports = new AtomicRef(List.empty[Transport])

  def linkTransport(t: Transport) =
    transports.transform(t +: _)

  def unlinkTransport(t: Transport) =
    transports.transform { _.filter(t ne _) }

  def withTransport[A](t: Transport)(f: =>A): A = {
    linkTransport(t)
    try f
    finally unlinkTransport(t)
  }

  def render: NodeSeq = NodeSeq.Empty
}

object Page {
  def apply(components: (Page => PageComponent)*) = new Page {
    lazy val pageComponents = components map (_(this))
  }
}

/**
 * A Page uniquely identifies a web page rendered with reactive-web components.
 * It is used to associate RElems and ReactionsComets.
 * An RElem can be associated with multiple Pages. The corresponding
 * element will be kept in sync in both places.
 */
trait Page extends Logger with IdCounter {
  case class QueueingJS[T: CanRender](pageId: String, transport: Transport, data: T) {
    def js: String = implicitly[CanRender[T]] apply data
  }
  case class DroppedNoTransport[T: CanRender](pageId: String, data: T) {
    def js: String = implicitly[CanRender[T]] apply data
  }

  /**
   * Use if you need to tie a listener's lifespan to the lifetime of the Page
   */
  implicit object observing extends Observing

  val id = randomString(20)

  def nextId = f"reactiveWebId_$id%s_$nextNumber%06d"

  def pageComponents: Seq[PageComponent]
  lazy val ajaxEvents = pageComponents.foldLeft(EventStream.empty[(String, JValue)])(_ | _.ajaxEvents)

  private val pageTransports = new AtomicRef(List.empty[Transport])


  /**
   * Queues javascript to be rendered via the available `Transport` with the highest priority
   */
  def queue[T: CanRender](renderable: T) = {
    val cmps = if(pageComponents ne null) pageComponents else Nil
    val transports = pageTransports.get ++ cmps.flatMap(_.transports.get)
    if(transports.isEmpty) warn(DroppedNoTransport(id, renderable))
    else {
      val preferredTransport = transports.maxBy(_.currentPriority)
      trace(QueueingJS(id, preferredTransport, renderable))
      preferredTransport.queue(renderable)
    }
  }

  def render: NodeSeq = pageComponents.flatMap(_.render)
}
