package reactive
package web
package html

import scala.xml.{ NodeSeq, Text }
import net.liftweb.http.js.JsCmds.SetHtml

/**
 * Represents a button element in the DOM
 */
trait Button extends RElem {
  /**
   * The click DOM event
   */
  val click = DomEventSource.click

  def baseElem = <button type={ buttonType.toString.toLowerCase }/>
  def events = List(click)
  def properties = Nil

  /**
   * The type of the button. One of ButtonType.Button, ButtonType.Submit, ButtonType.Reset
   */
  def buttonType: ButtonType.Value

  override def toString = "Button("+baseElem+")"
}

/**
 * Provides several factories for creating Buttons
 */
object Button {
  def disabled(init: Boolean = false)(implicit observing: Observing) = PropertyVar("disabled")(init)

  /**
   * Creates a Button Cell of the specified type and with the specified contents
   * @param buttonType the type of the button. Default is ButtonType.Button
   * @param content The contents of the button. Default is empty
   */
  def apply(buttonType: ButtonType.Value = ButtonType.Button, content: Signal[NodeSeq] = Val(NodeSeq.Empty))(implicit config: CanRenderDomMutationConfig): Button with Cell = {
    val content0 = content
    val buttonType0 = buttonType
    new Button with Cell {
      def renderer = config.domMutationRenderer
      override val buttonType = buttonType0
      val content = content0
    }
  }

  /**
   * Returns a function for Lift binding that renders a Button Cell whose contents are
   * bound by the function value of the Signal.
   * The function's input is passed to the value of the Signal.
   * For example:
   * "button" #> Button(intSignal map (i => "*" #> i))
   * @param binding the Signal[NodeSeq=>NodeSeq] that represents the bind function used to generate the contents of the Button.
   * @return a NodeSeq=>NodeSeq that on each invocation renders a new Span Button
   */
  def apply(buttonType: ButtonType.Value, binding: Signal[NodeSeq => NodeSeq])(implicit p: Page, config: CanRenderDomMutationConfig): NodeSeq => NodeSeq =
    bindFunc2contentFunc(binding)(apply(buttonType, _)(config).render)

  /**
   * Creates a Button Cell with the specified contents and with the specified handler function
   * Requires an Observing to be in the implicit scope (or to be passed in manually)
   * @param content The contents of the button. Default is empty
   * @param action the callback to invoke when the button is clicked
   */
  def apply(content: Signal[NodeSeq])(action: => Unit)(implicit observing: Observing, config: CanRenderDomMutationConfig): Button with Cell = {
    val ret = apply(ButtonType.Button, content)(config)
    ret.click.eventStream foreach {_=>action}
    ret
  }

  /**
   * Creates a Button with a String label and the specified handler function
   * Requires an Observing to be in the implicit scope or to be passed in manually
   * @param label The text of the button
   * @param action the callback to invoke when the button is clicked
   */
  def apply(label: String)(action: => Unit)(implicit observing: Observing, config: CanRenderDomMutationConfig): Button with Cell =
    apply(Val(Text(label)))(action)(observing, config)
}
/**
 * Enumerates the types of buttons (the type attribute of the button tag)
 */
object ButtonType extends Enumeration {
  val Button, Submit, Reset = Value
}
