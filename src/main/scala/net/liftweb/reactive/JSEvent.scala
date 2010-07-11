package net.liftweb.reactive

object JSEvent {
  def eventName[T <: JSEvent : Manifest] = manifest[T].erasure.getSimpleName.toLowerCase
}
sealed trait JSEvent

case class Modifiers(alt: Boolean, ctrl: Boolean, shift: Boolean, meta: Boolean)
case class Buttons(left: Boolean, middle: Boolean, right: Boolean, modifiers: Modifiers)
case class Position(client: (Int,Int))

case object Blur extends JSEvent
case object Change extends JSEvent
case class Click(modifiers: Modifiers) extends JSEvent
case class DblClick(modifiers: Modifiers) extends JSEvent
case object Error extends JSEvent
case object Focus extends JSEvent
case class KeyDown(code: Int, modifiers: Modifiers) extends JSEvent
case class KeyPress(code: Int, modifiers: Modifiers) extends JSEvent
case class KeyUp(code: Int, modifiers: Modifiers) extends JSEvent
case class MouseDown(buttons: Buttons, pos: Position) extends JSEvent
case class MouseMove(buttons: Buttons, pos: Position) extends JSEvent
case class MouseOut(buttons: Buttons, pos: Position, related: Option[RElem]) extends JSEvent
case class MouseOver(buttons: Buttons, pos: Position, related: Option[RElem]) extends JSEvent
case class MouseUp(buttons: Buttons, pos: Position) extends JSEvent
case object Resize extends JSEvent
case class Select(modifiers: Modifiers) extends JSEvent
case object Unload extends JSEvent
