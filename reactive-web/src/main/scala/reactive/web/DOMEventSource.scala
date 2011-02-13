package reactive
package web


import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.{JsExp, JE}
	import JE.JsRaw
import net.liftweb.util.Helpers.urlDecode

/**
 * Represents a DOM event type propagated to the server. 
 * Generates the javascript necessary for an event listener to
 * pass the event to the server.
 */
class DOMEventSource[T <: DOMEvent : Manifest] {
	/**
	 * The EventStream that represents the primary event data
	 */
  val eventStream = new EventSource[T] {}
  /**
   * The name of the event
   */
  def eventName = DOMEvent.eventName[T]
  /**
   * The name of the attribute to add the handler to
   */
  def attributeName = "on" + eventName
  
  //TODO maybe rename to rawEvent*?
  //TODO perhaps instead of managing the two event streams separately,
  // rather manage rawEventStream directly, and eventStream should
  // be derived from it via map.
  /**
   * Addition data can be sent with every event by putting a name and
   * a javascript expression in this Map
   */
  var extraEventData = Map[String, JsExp]()
  /**
   * The EventStram that contains all the data sent with the event
   */
  val extraEventStream = new EventSource[Map[String, String]] {}
  
  /**
   * The javascript to run whenever the browser fires the event, to
   * propagate the event to the server
   */
  def propagateJS: String = {
    def encodeEvent = {
      def modifiers =
        "'altKey='+event.altKey+';ctrlKey='+event.ctrlKey+';metaKey='+event.metaKey+';shiftKey='+event.shiftKey"
      def buttons = "'button='+event.button+';'+" + modifiers
      def position = "'clientX='+event.clientX+';clientY='+event.clientY"
      def mouse = position + "+';'+" + buttons
      def key = "'code='+(event.keyCode||event.charCode)+';'+" + modifiers
      def relatedTarget = "'related='+encodeURIComponent(event.relatedTarget.id)"
      def fromElement = "'related='+encodeURIComponent(event.fromElement.id)"
      def toElement = "'related='+encodeURIComponent(event.toElement.id)"
      def out = mouse + "+';'+" + (
        if(S.request.dmap(false)(_.isIE)) toElement else relatedTarget
      )
      def over = mouse + "+';'+" + (
        if(S.request.dmap(false)(_.isIE)) fromElement else relatedTarget
      )
      val eventEncoding = if(!eventStream.hasListeners) {
        "''"
      } else eventName match {
        case "blur" | "change" | "error" | "focus" | "resize" | "unload" => ""
        case "click" | "dblclick" | "select" => modifiers
        case "keydown" | "keypress" | "keyup" => key
        case "mousedown" | "mousemove" | "mouseup" => mouse
        case "mouseout" => out
        case "mouseover" => over
      }
      if(!extraEventStream.hasListeners)
        eventEncoding
      else extraEventData.foldLeft(eventEncoding){
        case (encoding, (key, expr)) =>
          //  xxx + ';key=' + encodeURIComponent(expr)
          encoding + "+';" + key + "='+encodeURIComponent(" + expr.toJsCmd + ")"
      }
    }
    
    def decodeEvent(evt: Map[String,String]): T = {
      def bool(s: String) = evt(s) match { case "false"|"undefined" => false; case "true" => true }
      def modifiers = Modifiers(bool("altKey"), bool("ctrlKey"), bool("shiftKey"), bool("metaKey"))
      def buttons = {
        val b = evt("buttons").toInt
        if(S.request.dmap(false)(_.isIE))
          Buttons((b&1)!=0,(b&4)!=0,(b&2)!=0, modifiers)
        else
          Buttons(b==0,b==1,b==2, modifiers)
      }
      def position = Position((evt("clientX").toInt, evt("clientY").toInt))
      
      (eventName match {
        case "blur" => Blur
        case "change" => Change
        case "click" => Click(modifiers)
        case "dblclick" => DblClick(modifiers)
        case "error" => Error
        case "focus" => Focus
        case "keydown" => KeyDown(evt("code").toInt, modifiers)
        case "keyup" => KeyUp(evt("code").toInt, modifiers)
        case "keypress" => KeyPress(evt("code").toInt, modifiers)
        case "mousedown" => MouseDown(buttons, position)
        case "mousemove" => MouseMove(buttons, position)
        case "mouseup" => MouseUp(buttons, position)
        case "mouseover" => MouseOver(buttons, position, RElem.elems.get(evt("related")))
        case "mouseout" => MouseOut(buttons, position, RElem.elems.get(evt("related")))
        case "resize" => Resize
        case "select" => SelectText(modifiers)
        case "unload" => Unload
      }).asInstanceOf[T]
    }
    def handler(s: String) = {
      val evt: Map[String,String] = Map(
        s.split(";").toList.flatMap {
          _.split("=").toList match {
            case property :: value :: Nil => Some((property, urlDecode(value)))
            case property :: Nil => Some((property, ""))
            case _ => None
          }
        }: _*
      )
      println(eventName + ": Received encoding event: " + evt)
      if(eventStream.hasListeners) try {
        eventStream.fire(decodeEvent(evt))
      } catch {
        case e: java.util.NoSuchElementException =>
          System.err.println(eventName + " has listeners but caught exception while decoding event:")
          e.printStackTrace()
      }
      extraEventStream.fire(evt)
    }
    S.fmapFunc(S.contextFuncBuilder(RElem.ajaxFunc(handler))) {funcId =>
      SHtml.makeAjaxCall(
        JsRaw("'"+funcId+"=' + encodeURIComponent("+encodeEvent+")")
      ).toJsCmd
    }
  }
  
  /**
   * Returns an attribute that will register a handler with the event
   */
  def asAttribute: xml.MetaData = if(eventStream.hasListeners || extraEventStream.hasListeners) {
    new xml.UnprefixedAttribute(
      attributeName,
      propagateJS,
      xml.Null
    )
  } else {
    xml.Null
  }
}



