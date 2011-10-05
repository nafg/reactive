package reactive
package web
package html


/**
 * Represents a text input field in the DOM
 * @param _value a Var used to update the value of the value property
 */
//TODO should we be using Vars so much? Maybe in & out Signals?
class TextInput(val initialValue: String = "")(implicit observing: Observing) extends RElem {
  /**
   * The doubleclick DOM event
   */
  val dblClick = DomEventSource.dblClick

  /**
   * The keyup DOM event
   */
  val keyUp = DomEventSource.keyUp

  /**
   * The change DOM event
   */
  val change = DomEventSource.change

  /**
   * The value DOM attribute/property. The contents of the input field.
   */
  val value = TextInput.value(initialValue)

  /**
   * The size attribute/property. The width, in characters, of the input.
   */
  val size = TextInput.size(None)

  def events = List(dblClick, keyUp, change)
  def properties = List(value, size)
  def baseElem = <input type="text" />
}

/**
 * Provides several factories for creating TextInputs.
 */
object TextInput {
  /**
   * The value DOM attribute/property. The contents of the input field.
   * @param v the Var[String] to synchronize with the property
   */
  def value(init: String = "")(implicit observing: Observing) = PropertyVar("value")(init)(PropertyCodec.string, observing)

  /**
   * The size DOM attribute/property. The width, in characters, of the input.
   * @param v the Var[String] to synchronize with the property
   */
  def size(init: Option[Int] = None)(implicit observing: Observing) = PropertyVar("size")(init)(PropertyCodec.intOption, observing)

  /**
   * Create a TextInput whose value property is kept in a Var.
   * By default, its value is updated only on change events. To receive
   * updates even on keyup events, write field.value.updateOn(field.keyUp).
   * @param value the Var to keep the TextInput's value in
   */
  def apply(value: Var[String] = Var(""))(implicit o: Observing): TextInput = {
    var ret = new TextInput
    ret.value updateOn ret.change
    value <--> ret.value
    ret
  }

  /**
   * Create a TextInput with the initial value, and a function that is called whenever the value changes.
   * Requires an Observing to be in the implicit scope, or to be passed in manually.
   * @param default the initial contents of the field
   * @param setter the function to call whenever the value changes.
   */
  def apply(default: String)(setter: String=>Unit)(implicit observing: Observing): TextInput = {
    val ret = new TextInput(default)
    ret.value =>> setter
    ret
  }
}

