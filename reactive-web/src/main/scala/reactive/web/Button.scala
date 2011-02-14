package reactive
package web

import scala.xml.{NodeSeq, Text}
import net.liftweb.http.js.JsCmds.SetHtml


/**
 * Represents a button element in the DOM
 * @param buttonType the type of the button. Default is ButtonType.Button
 * @param content The contents of the button. Default is empty
 */
//TODO make use of Cell
class Button(buttonType: ButtonType.Value = ButtonType.Button, content: Signal[NodeSeq] = Val(NodeSeq.Empty)) extends RElem {
  /**
   * The click DOM event
   */
  val click = new DOMEventSource[Click]
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
  override def toString = "Button(" + baseElem + ")"
}


/**
 * Provides several factories for creating Buttons
 */
object Button {
  /**
   * Creates a button of the specified type and with the specified contents
   * @param buttonType the type of the button. Default is ButtonType.Button
   * @param content The contents of the button. Default is empty
   */
  def apply(buttonType: ButtonType.Value=ButtonType.Button, content: Signal[NodeSeq] = Val(NodeSeq.Empty)) = new Button(buttonType,content)
  /**
   * Creates a button with the specified contents and with the specified handler function
   * Requires an Observing to be in the implicit scope or to be passed in manually
   * @param content The contents of the button. Default is empty
   * @param action the callback to invoke when the button is clicked
   */
  def apply(
    content: Signal[NodeSeq]
  )(
    action: =>Unit
  )(
    implicit observing: Observing
  ) = new Button(ButtonType.Button,content) {
    click.eventStream foreach {_=>action}
  }
  /**
   * Creates a button with a String label and the specified handler function
   * Requires an Observing to be in the implicit scope or to be passed in manually
   * @param label The text of the button
   * @param action the callback to invoke when the button is clicked
   */
  def apply(label: String)(action: =>Unit)(implicit observing: Observing) = new Button(ButtonType.Button, Val(Text(label))) {
    click.eventStream foreach {_=>action}
  }
}
/**
 * Enumerates the types of buttons (the type attribute of the button tag)
 */
object ButtonType extends Enumeration {
  val Button, Submit, Reset = Value
}
