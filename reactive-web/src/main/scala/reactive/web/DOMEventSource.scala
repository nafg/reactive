package reactive
package web

import net.liftweb.http.{ S, SHtml }
import net.liftweb.util.Helpers.urlDecode
import javascript._
import JsTypes._

import scala.xml.{ Elem, NodeSeq }

/**
 * Represents a DOM event type and related JsEventStreams.
 * Generates the javascript necessary to fire JsEventStreams
 * in response to events.
 */
//TODO better name--it is not an EventSource; only wraps a JsEventStream

class DOMEventSource[T <: DOMEvent:Manifest:EventEncoder]
extends (NodeSeq => NodeSeq) with Forwardable[T] with Logger with JsForwardable[JsObj] {
  case class ReceivedEncodedEvent(event: Map[String, String]) extends LogEventPredicate
  case class CaughtExceptionDecodingEvent(event: Map[String, String], exception: Exception) extends LogEventPredicate

  /**
   * The JsEventStream that fires the primary event data
   */
  val jsEventStream = new JsEventStream[JsObj]
  /**
   * An EventStream that fires events of type T on the server
   */
  lazy val eventStream: EventStream[T] =
    jsEventStream.toServer

  /**
   * The name of the event
   */
  def eventName = DOMEvent.eventName[T]
  
  /**
   * The name of the attribute to add the handler to
   */
  def attributeName = "on"+eventName

  case class EventData[T<:JsAny](encode: $[T], es: JsEventStream[T])
  private var eventData: List[EventData[_]] = List(
    EventData(implicitly[EventEncoder[T]].encodeExp, jsEventStream)
  )
  
  def addEventData[T<:JsAny](jsExp: $[T], es: JsEventStream[T]) = synchronized {
    eventData ::= EventData(jsExp, es)
  }
  

  /**
   * The javascript to run whenever the browser fires the event, to
   * propagate the event to the server
   */
  def propagateJS: String = {
    eventData.map{ case EventData(enc, es) =>
      es.fireExp(enc).render
    }.mkString(";") + ";reactive.doAjax()"
  }

  /**
   * Returns an attribute that will register a handler with the event
   */
  def asAttribute: xml.MetaData = new xml.UnprefixedAttribute(
    attributeName,
    propagateJS,
    xml.Null
  )

  def apply(elem: Elem): Elem = {
    val a = asAttribute
    elem.attribute(a.key) match {
      case None => elem % asAttribute
      case Some(ns) => elem % new xml.UnprefixedAttribute(a.key, ns.text+";"+a.value.text, xml.Null)
    }
  }
  def apply(in: NodeSeq): NodeSeq = apply(nodeSeqToElem(in))

  def foreach(f: T => Unit)(implicit o: Observing) = eventStream.foreach(f)(o)
  def foreach[E[J <: JsAny] <: $[J], F: ToJs.To[JsObj=|>JsVoid, E]#From](f: F) = jsEventStream.foreach(f)
  def foreach(f: $[JsObj =|> JsVoid]) = jsEventStream.foreach(f)

  override def toString = "DOMEventSource["+manifest[T]+"]"
}

object DOMEventSource {
  /**
   * Creates a new Click DOMEventSource
   */
  def click = new DOMEventSource[Click]
  /**
   * Creates a new DblClick DOMEventSource
   */
  def dblClick = new DOMEventSource[DblClick]
  /**
   * Creates a new KeyUp DOMEventSource
   */
  def keyUp = new DOMEventSource[KeyUp]
  /**
   * Creates a new Change DOMEventSource
   */
  def change = new DOMEventSource[Change.type]

}
