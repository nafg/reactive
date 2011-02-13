package reactive
package web


/**
 * Provides a utility method to get a javascript event name from a Manifest of a DOMEvent
 */
object DOMEvent {
  def eventName[T <: DOMEvent : Manifest] = manifest[T].erasure.getSimpleName.toLowerCase match {
    case s if s endsWith "$" => s.substring(0, s.length-1)
    case s => s
  }
}

/**
 * The base class for all events
 */
sealed trait DOMEvent
case object Blur extends DOMEvent
case object Change extends DOMEvent
case class Click(modifiers: Modifiers) extends DOMEvent
case class DblClick(modifiers: Modifiers) extends DOMEvent
case object Error extends DOMEvent
case object Focus extends DOMEvent
/**
 * @param code the keyCode or charCode property of the javascript event object
 */
case class KeyDown(code: Int, modifiers: Modifiers) extends DOMEvent
/**
 * @param code the keyCode or charCode property of the javascript event object
 */
case class KeyPress(code: Int, modifiers: Modifiers) extends DOMEvent
/**
 * @param code the keyCode or charCode property of the javascript event object
 */
case class KeyUp(code: Int, modifiers: Modifiers) extends DOMEvent
case class MouseDown(buttons: Buttons, pos: Position) extends DOMEvent
case class MouseMove(buttons: Buttons, pos: Position) extends DOMEvent
/**
 * @param related the RElem corresponding to the relatedTarget or toElement property of the event object
 */
case class MouseOut(buttons: Buttons, pos: Position, related: Option[RElem]) extends DOMEvent
/**
 * @param related the RElem corresponding to the relatedTarget or fromElement property of the event object
 */
case class MouseOver(buttons: Buttons, pos: Position, related: Option[RElem]) extends DOMEvent
case class MouseUp(buttons: Buttons, pos: Position) extends DOMEvent
case object Resize extends DOMEvent
case class SelectText(modifiers: Modifiers) extends DOMEvent
case object Unload extends DOMEvent

/**
 * Encapsulates the state of modifier keys on the keyboard at the time of this event
 */
case class Modifiers(alt: Boolean, ctrl: Boolean, shift: Boolean, meta: Boolean)
/**
 * Encapsulates the mouse buttons used to generate the event along with the modifier keys
 */
case class Buttons(left: Boolean, middle: Boolean, right: Boolean, modifiers: Modifiers)
/**
 * Encapsulates a mouse pointer position
 */
//TODO remove? just use Tuple2?
case class Position(client: (Int,Int))
