package reactive
package web

import javascript._
import JsTypes._
import net.liftweb.json.Serialization
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.DefaultFormats

/**
 * Implicit instances of EventEncoder are associated with DomEvents
 * to provide a javascript object initializer expression
 * that consists of the relevant data in the browser's Event object.
 */
//TODO just use FromJs
class EventEncoder[T <: DomEvent](val encodeExp: $[JsObj] => $[JsObj])

object EventEncoder {
  val empty = Map.empty.$
  implicit val blur: EventEncoder[Blur] = new EventEncoder[Blur](_ => empty)
  implicit val change: EventEncoder[Change] = new EventEncoder[Change](_ => empty)
  implicit val error: EventEncoder[Error] = new EventEncoder[Error](_ => empty)
  implicit val focus: EventEncoder[Focus] = new EventEncoder[Focus](_ => empty)
  implicit val resize: EventEncoder[Resize] = new EventEncoder[Resize](_ => empty)
  implicit val unload: EventEncoder[Unload] = new EventEncoder[Unload](_ => empty)

  def modifiers(event: $[JsObj]) = javascript.Object(
    "alt" -> (event >> 'altKey),
    "ctrl" -> (event >> 'ctrlKey),
    "shift" -> (event >> 'shiftKey),
    "meta" -> (event >> 'metaKey)
  )
  def modifiersOnly(event: $[JsObj]) = javascript.Object("modifiers" -> modifiers(event))
  implicit val click: EventEncoder[Click] = new EventEncoder[Click](modifiersOnly)
  implicit val dblClick: EventEncoder[DblClick] = new EventEncoder[DblClick](modifiersOnly)
  implicit val selectText: EventEncoder[SelectText] = new EventEncoder[SelectText](modifiersOnly)

  def key(event: $[JsObj]) = javascript.Object(
    "code" -> ((event >> 'keyCode) || (event >> 'charCode)),
    "modifiers" -> modifiers(event)
  )
  implicit val keyDown: EventEncoder[KeyDown] = new EventEncoder[KeyDown](key)
  implicit val keyUp: EventEncoder[KeyUp] = new EventEncoder[KeyUp](key)
  implicit val keyPress: EventEncoder[KeyPress] = new EventEncoder[KeyPress](key)

  case class CalcIsIE(f: () => Boolean) {
    def apply() = f()
  }

  def buttons(event: $[JsObj])(implicit isIE: CalcIsIE = CalcIsIE(() => false)) = if (isIE()) javascript.Object(
    "left" -> ((event >> 'buttons) & 1 !== 0),
    "middle" -> ((event >> 'buttons) & 4 !== 0),
    "right" -> ((event >> 'buttons) & 2 !== 0),
    "modifiers" -> modifiers(event)
  )
  else javascript.Object(
    "left" -> ((event >> 'buttons) === 0),
    "middle" -> ((event >> 'buttons) === 1),
    "right" -> ((event >> 'buttons) === 2),
    "modifiers" -> modifiers(event)
  )
  def mouse(event: $[JsObj]) = javascript.Object(
    "buttons" -> buttons(event),
    "pos" -> javascript.Object(
      "x" -> (event >> 'clientX),
      "y" -> (event >> 'clientY)
    )
  )
  implicit def mouseDown: EventEncoder[MouseDown] = new EventEncoder[MouseDown](mouse)
  implicit def mouseUp: EventEncoder[MouseUp] = new EventEncoder[MouseUp](mouse)
  implicit def mouseMove: EventEncoder[MouseMove] = new EventEncoder[MouseMove](mouse)

  implicit def mouseOut: EventEncoder[MouseOut] = new EventEncoder[MouseOut](mouse) //TODO related
  implicit def mouseOver: EventEncoder[MouseOver] = new EventEncoder[MouseOver](mouse) //TODO related
}

object DomEvent {
  implicit def fromJs[E <: DomEvent: Manifest: EventEncoder]: FromJs[JsObj, E] = new FromJs[JsObj, E](
    e => implicitly[EventEncoder[E]].encodeExp(e),
    _.extract(DefaultFormats, manifest[E])
  )
}

/**
 * The base class for all events
 */
sealed trait DomEvent
case class Blur() extends DomEvent
case class Change() extends DomEvent
case class Click(modifiers: Modifiers = Modifiers()) extends DomEvent
case class DblClick(modifiers: Modifiers = Modifiers()) extends DomEvent
case class Error() extends DomEvent
case class Focus() extends DomEvent
/**
 * @param code the keyCode or charCode property of the javascript event object
 */
case class KeyDown(code: Int, modifiers: Modifiers = Modifiers()) extends DomEvent
/**
 * @param code the keyCode or charCode property of the javascript event object
 */
case class KeyPress(code: Int, modifiers: Modifiers = Modifiers()) extends DomEvent
/**
 * @param code the keyCode or charCode property of the javascript event object
 */
case class KeyUp(code: Int, modifiers: Modifiers = Modifiers()) extends DomEvent

case class MouseDown(buttons: Buttons, pos: Position) extends DomEvent
case class MouseMove(buttons: Buttons, pos: Position) extends DomEvent
/**
 * @param related the RElem corresponding to the relatedTarget or toElement property of the event object
 */
case class MouseOut(buttons: Buttons, pos: Position, related: Option[RElem]) extends DomEvent
/**
 * @param related the RElem corresponding to the relatedTarget or fromElement property of the event object
 */
case class MouseOver(buttons: Buttons, pos: Position, related: Option[RElem]) extends DomEvent
case class MouseUp(buttons: Buttons, pos: Position) extends DomEvent
case class Resize() extends DomEvent
case class SelectText(modifiers: Modifiers) extends DomEvent
case class Unload() extends DomEvent

/**
 * Encapsulates the state of modifier keys on the keyboard at the time of this event
 */
case class Modifiers(alt: Boolean = false, ctrl: Boolean = false, shift: Boolean = false, meta: Boolean = false)
/**
 * Encapsulates the mouse buttons used to generate the event along with the modifier keys
 */
case class Buttons(left: Boolean, middle: Boolean, right: Boolean, modifiers: Modifiers = Modifiers())
/**
 * Encapsulates a mouse pointer position
 */
//TODO remove? just use Tuple2?
case class Position(x: Int, y: Int)
