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
   * By default, it is updated only on change events. To receive
   * updates even on keyup events, write field.value.updateOn(field.change).
   */
  val value = new DOMStringProperty {
    def name = "value"
    def elemId = id
    val value = _value
    
    this updateOn change
  }
  /**
   * The size attribute/property. The width, in characters, of the input.
   */
  val size = new DOMIntProperty {
    def name = "size"
    def elemId = id
    val value = Var(-1)
  }
  
  def events = List(dblClick, keyUp, change)
  def properties = List(value, size)
  def baseElem = <input type="text" />
}

/**
 * Provides several factories for creating TextInputs.
 */
object TextInput {
  /**
   * Create a TextInput whose value property is kept in a Var.
   * @param value the Var to keep the TextInput's value in
   */
  def apply(value: Var[String] = Var("")) = new TextInput(value)
  /**
   * Create a TextInput with the initial value, and a function that is called whenever the value changes.
   * Requires an Observing to be in the implicit scope, or to be passed in manually.
   * @param default the initial contents of the field
   * @param setter the function to call whenever the value changes.
   */
  def apply(default: String)(setter: String=>Unit)(implicit observing: Observing) = {
    val v = Var(default)
    v.change foreach setter
    new TextInput(v)
  }
}

