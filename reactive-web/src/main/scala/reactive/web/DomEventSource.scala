package reactive
package web

import net.liftweb.http.{ S, SHtml }
import net.liftweb.util.Helpers.urlDecode
import javascript._
import JsTypes._

import scala.xml.{ Elem, NodeSeq, UnprefixedAttribute, MetaData }

import scala.collection.mutable.WeakHashMap

/**
 * Represents a DOM event type and related JsEventStreams.
 * Generates the javascript necessary to fire JsEventStreams
 * in response to events.
 */
//TODO better name? It is not an EventSource; only wraps a JsEventStream
class DomEventSource[T <: DomEvent: Manifest: EventEncoder] extends Forwardable[T] with Logger with JsForwardable[JsObj] {
  /**
   * Adds asAttribute to an Elem.
   * If an attribute exists with the same name, combine the two values,
   * separated by a semicolon.
   */
  class Renderer(implicit page: Page)
    extends ElemFuncWrapper({ elem =>
      val a = asAttribute
      elem.attribute(a.key) match {
        case None     => elem % a
        case Some(ns) => elem % new xml.UnprefixedAttribute(a.key, ns.text+";"+a.value.text, xml.Null)
      }
    })

  /**
   * The JsEventStream that fires the primary event data
   */
  def jsEventStream(implicit p: Page) = getEventObjectData(p).es
  /**
   * An EventStream that fires events of type T on the server.
   * Calls toServer on jsEventStream.
   */
  def eventStream(implicit page: Page): EventStream[T] =
    jsEventStream(page).toServer

  /**
   * The name of the event
   */
  def eventName = scalaClassName(manifest[T].erasure).toLowerCase

  /**
   * The name of the attribute to add the handler to
   */
  def attributeName = "on"+eventName

  /**
   * Pairs a javascript expression to fire when this event occurs, with
   * a javascript event stream to fire it from.
   */
  case class EventData[T <: JsAny](encode: $[T], es: JsEventStream[T])

  private val eventData = WeakHashMap[Page, List[EventData[_]]]()
  private def getEventObjectData(implicit p: Page) = eventObjectData.getOrElseUpdate(p, EventData(implicitly[EventEncoder[T]].encodeExp, new JsEventStream[JsObj]))
  private val eventObjectData = WeakHashMap[Page, EventData[JsObj]]()

  /**
   * Register data to be fired whenever this event occurs on the specified page
   * @param jsExp the javascript to be evaluated when it occurs
   * @param es the JsEventStream that the value will be fired from
   */
  def addEventData[T <: JsAny](jsExp: $[T], es: JsEventStream[T])(implicit page: Page) = eventData.synchronized {
    val ed = EventData(jsExp, es)
    eventData(page) = eventData.get(page) match {
      case Some(eds) =>
        eds :+ ed
      case None => ed :: Nil
    }
  }

  /**
   * The javascript to run whenever the browser fires the event.
   * Whether this will result in an ajax call to the server
   * depends on the JsEventStreams registered with this DomEventSource
   * (for instance, whether toServer has been called on them,
   * such as by calling DomEventSource#eventStream),
   * propagate the event to the server
   */
  def propagateJS(implicit page: Page): String = {
    (getEventObjectData(page) :: eventData.getOrElse(page, Nil)).map{
      case EventData(enc, es) => JsExp render es.fireExp(enc)
    }.mkString(";")+";reactive.doAjax()"
  }

  /**
   * Returns an attribute that will register a handler with the event.
   * Combines attributeName and propagateJS in a scala.xml.MetaData.
   */
  def asAttribute(implicit page: Page): MetaData = new UnprefixedAttribute(
    attributeName,
    propagateJS,
    xml.Null
  )

  def render(implicit page: Page) = new Renderer()(page)

  /**
   * Calls eventStream.foreach
   */
  def foreach(f: T => Unit)(implicit o: Observing) = eventStream.foreach(f)(o)
  /**
   * Calls jsEventStream.foreach
   */
  def foreach[E[J <: JsAny] <: $[J], F: ToJs.To[JsObj =|> JsVoid, E]#From](f: F) = jsEventStream.foreach(f)
  /**
   * Calls jsEventStream.foreach
   */
  def foreach(f: $[JsObj =|> JsVoid]) = jsEventStream.foreach(f)

  override def toString = "DomEventSource["+manifest[T]+"]"
}

object DomEventSource {
  /**
   * An implicit conversion from DomEventSource to NodeSeq=>NodeSeq. Requires an implicit Page. Calls render.
   */
  implicit def toNodeSeqFunc(des: DomEventSource[_])(implicit page: Page): NodeSeq => NodeSeq = des.render(page)

  /**
   * Creates a new Click DomEventSource
   */
  def click = new DomEventSource[Click]
  /**
   * Creates a new DblClick DomEventSource
   */
  def dblClick = new DomEventSource[DblClick]
  /**
   * Creates a new KeyUp DomEventSource
   */
  def keyUp = new DomEventSource[KeyUp]
  /**
   * Creates a new Change DomEventSource
   */
  def change = new DomEventSource[Change]

}
