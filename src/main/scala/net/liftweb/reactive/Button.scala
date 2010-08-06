package net.liftweb.reactive

import _root_.reactive._
import scala.xml.{NodeSeq, Text}
import net.liftweb.http.js.JsCmds.SetHtml

class Button(buttonType: ButtonType.Value = ButtonType.Button, content: SignalBase[NodeSeq] = Val(NodeSeq.Empty)) extends RElem {
  val click = new JSEventSource[Click]
  def baseElem = <button type={buttonType.toString.toLowerCase}>{content.now}</button>
  def events = List(click)
  def properties = Nil
  override def addPage(implicit page: Page) {
    super.addPage(page)
    content foreach {s =>
      Reactions.inAnyScope(page) {
        Reactions.queue(SetHtml(id, s))
      }
    }
  }
  override def toString = "TextSpan(" + render + ")"
}

object Button {
  def apply(buttonType: ButtonType.Value=ButtonType.Button,content: Signal[NodeSeq] = Val(NodeSeq.Empty)) = new Button(buttonType,content)
  def apply(
    content: SignalBase[NodeSeq]
  )(
    action: =>Unit
  )(
    implicit observing: Observing
  ) = new Button(ButtonType.Button,content) {
    click.eventStream foreach {_=>action}
  }
  def apply(label: String)(action: =>Unit)(implicit observing: Observing) = new Button(ButtonType.Button, Val(Text(label))) {
    click.eventStream foreach {_=>action}
  }
}
object ButtonType extends Enumeration {
  val Button, Submit, Reset = Value
}