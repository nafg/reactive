package reactive
package web

import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.{ClassTag, classTag}
import scala.xml.{MetaData, NodeSeq, UnprefixedAttribute}

import reactive.Util.scalaClassName
import reactive.logging.Logger
import reactive.web.javascript.JsTypes._
import reactive.web.javascript._

class DomEventSourceCanForeach[T <: DomEvent](domEventSource: DomEventSource[T])(page: Page) extends Forwardable[T, DomEventSource[T]] with JsForwardable[JsObj] {
  def self = domEventSource

  /**
   * Calls eventStream.foreach
   */
  def foreach(f: T => Unit)(implicit o: Observing) = {
    domEventSource.eventStream(page).foreach(f)(o)
    self
  }

  /**
   * Calls jsEventStream.foreach
   */
  def foreach[E[J <: JsAny] <: $[J], F: ToJs.To[JsObj =|> JsVoid, E]#From](f: F): Unit =
    domEventSource.jsEventStream(page).foreach(f)
  /**
   * Calls jsEventStream.foreach
   */
  def foreach(f: $[JsObj =|> JsVoid]): Unit = domEventSource.jsEventStream(page).foreach(f)
}

/**
 * Represents a DOM event type and related JsEventStreams.
 * Generates the javascript necessary to fire JsEventStreams
 * in response to events.
 */
//TODO better name? It is not an EventSource; only wraps a JsEventStream
class DomEventSource[T <: DomEvent: ClassTag: EventEncoder] extends Logger {
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
    jsEventStream(page).toServer[T](manifest = scala.reflect.Manifest.classType[T](classTag[T].runtimeClass))

  /**
   * The name of the event
   */
  def eventName = scalaClassName(classTag[T].runtimeClass).toLowerCase

  /**
   * The name of the attribute to add the handler to
   */
  def attributeName = "on"+eventName

  /**
   * Pairs a javascript expression to fire when this event occurs, with
   * a javascript event stream to fire it from.
   */
  case class EventData[U <: JsAny](encode: $[U], es: JsEventStream[U])

  private val eventData = mutable.WeakHashMap[Page, List[EventData[_ <: JsAny]]]()
  private def getEventObjectData(implicit p: Page) = eventObjectData.getOrElseUpdate(p, EventData(implicitly[EventEncoder[T]].encodeExp(Symbol("event").$), new JsEventStream[JsObj]))

  private val eventObjectData = mutable.WeakHashMap[Page, EventData[JsObj]]()

  /**
   * Register data to be fired whenever this event occurs on the specified page
   * @param jsExp the javascript to be evaluated when it occurs
   * @param es the JsEventStream that the value will be fired from
   */
  def addEventData[U <: JsAny](jsExp: $[U], es: JsEventStream[U])(implicit page: Page): Unit = eventData.synchronized {
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
  def js(implicit page: Page): String = {
    (getEventObjectData(page) :: eventData.getOrElse(page, Nil)).map{
      case EventData(enc, es) => JsExp render buildJs(es.fireExp(enc))
    }.mkString(";")+";reactive.doAjax('"+page.id+"')"
  }

  /**
   * Returns an attribute that will register a handler with the event.
   * Combines attributeName and propagateJS in a scala.xml.MetaData.
   */
  def asAttribute(implicit page: Page): MetaData = new UnprefixedAttribute(
    attributeName,
    js,
    xml.Null
  )

  def render(implicit page: Page) = new Renderer()(page)

  override def toString = "DomEventSource["+classTag[T]+"]"
}

object DomEventSource {
  /**
   * An implicit conversion from DomEventSource to NodeSeq=>NodeSeq. Requires an implicit Page. Calls render.
   */
  implicit class toNodeSeqFunc(des: DomEventSource[_])(implicit page: Page) extends (NodeSeq => NodeSeq) {
    def apply(ns: NodeSeq) = des.render(page)(ns)
  }

  /**
   * Implicitly provides the `foreach` method and `Forwardable` methods.
   */
  implicit class canForeach[T <: DomEvent](des: DomEventSource[T])(implicit page: Page) extends DomEventSourceCanForeach(des)(page)

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
