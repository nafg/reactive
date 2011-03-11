package reactive
package web
package html


/**
 * Represents a text input field in the DOM
 * @param _value a Var used to update the value of the value property
 */
//TODO should we be using Vars so much? Maybe in & out Signals?
class TextInput(
  _value: Var[String] = Var("")
) extends RElem {
  /**
   * The doubleclick DOM event
   */
  val dblClick = new DOMEventSource[DblClick]
  /**
   * The keyup DOM event
   */
  val keyUp = new DOMEventSource[KeyUp]
  /**
   * The change DOM event
   */
  val change = new DOMEventSource[Change.type]
  /**
   * The value DOM attribute/property. The contents of the input field.
   */
  val value = TextInput.value(_value)
  /**
   * The size attribute/property. The width, in characters, of the input.
   */
  val size = TextInput.size(Var(-1))
  
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
  def value(v: Var[String]) = new DOMStringProperty {
    def name = "value"
    val value = v
  }
  /**
   * The size attribute/property. The width, in characters, of the input.
   * @param v the Var[String] to synchronize with the property
   */
  def size(v: Var[Int]) = new DOMIntProperty {
    def name = "size"
    val value = v
  }
  
  /**
   * Create a TextInput whose value property is kept in a Var.
   * By default, its value is updated only on change events. To receive
   * updates even on keyup events, write field.value.updateOn(field.keyUp).
   * @param value the Var to keep the TextInput's value in
   */
  def apply(value: Var[String] = Var("")): TextInput = {
    var ret = new TextInput(value)
    ret.value updateOn ret.change
    ret
  }
  /**
   * Create a TextInput with the initial value, and a function that is called whenever the value changes.
   * Requires an Observing to be in the implicit scope, or to be passed in manually.
   * @param default the initial contents of the field
   * @param setter the function to call whenever the value changes.
   */
  def apply(default: String)(setter: String=>Unit)(implicit observing: Observing): TextInput = {
    val v = Var(default)
    v.change foreach setter
    apply(v)
  }
}

