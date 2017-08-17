package reactive
package web

import scala.xml.NodeSeq

import net.liftweb.json.JsonAST.JValue

import reactive.logging.Logger

trait IdCounter {
  protected val counter = new java.util.concurrent.atomic.AtomicInteger(0)

  def nextNumber = counter.getAndIncrement
}

object Page {
  def apply(ttypes: (Page => TransportType)*): Page = new DfltPage(ttypes)
  private final class DfltPage(ttypes: Seq[Page => TransportType]) extends Page {
    private var initted = false
    lazy val transportTypes = {
      val ret = ttypes map (_(this))
      initted = true
      ret
    }
    override def toString = {
      val ttStr = if(!initted) "?" else transportTypes.map(_.getClass.getSimpleName).mkString(",")
      s"Page[id = $id, [$ttStr]]"
    }
  }
}

/**
 * A Page uniquely identifies a web page rendered with reactive-web components.
 * It is used to associate RElems and ReactionsComets.
 * An RElem can be associated with multiple Pages. The corresponding
 * element will be kept in sync in both places.
 */
trait Page extends Logger with IdCounter {
  case class QueueingJS(pageId: String, transport: Transport, data: Renderable) {
    @deprecated("Use `data` directly", "0.4.0")
    def js: String = data.render
  }
  case class DroppedNoTransport(pageId: String, data: Renderable) {
    @deprecated("Use `data` directly", "0.4.0")
    def js: String = data.render
  }

  /**
   * Use if you need to tie a listener's lifespan to the lifetime of the Page
   */
  implicit object observing extends Observing

  val id = (1 to 20).map(_ => (scala.util.Random.nextInt(26) + 'A').toChar).mkString

  def nextId = f"reactiveWebId_$id%s_$nextNumber%06d"

  def transportTypes: Seq[TransportType]
  lazy val ajaxEvents = transportTypes.foldLeft(EventStream.empty[(String, JValue)])(_ | _.ajaxEvents)

  private val pageTransports = new AtomicRef(List.empty[Transport])


  /**
   * Queues javascript to be rendered via whichever available [[Transport ]]has the highest priority
   */
  def queue[T](renderable: T)(implicit canRender: CanRender[T]) = {
    val tts = Option(transportTypes) getOrElse Nil
    val transports = pageTransports.get ++ tts.flatMap(_.transports.get)
    if(transports.isEmpty) warn(DroppedNoTransport(id, canRender(renderable)))
    else {
      val preferredTransport = transports.maxBy(_.currentPriority)
      trace(QueueingJS(id, preferredTransport, canRender(renderable)))
      preferredTransport.queued.fire(canRender(renderable))
    }
  }

  def render: NodeSeq = transportTypes.flatMap(_.render)
}
