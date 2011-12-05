package reactive
package web
package html


/**
 * Represents a checkbox input in the DOM
 * @param _value a Var that represents the value of the checkbox
 */
class CheckboxInput(_value: Var[Boolean] = Var(false))(implicit observing: Observing) extends RElem {
  /**
   * The dblclick DOM event
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
   * The checked property. Whether the checkbox is checked.
   */
  val checked = CheckboxInput.checked(_value.now) <--> _value updateOn change
  
  def events = List(dblClick, keyUp, change)
  def properties = List(checked)
  def baseElem = <input type="checkbox"/>
}

/**
 * Provides several factories for creating CheckboxInputs
 */
object CheckboxInput {
  def checked(init: Boolean = false)(implicit observing: Observing): PropertyVar[Boolean] = PropertyVar("checked")(init)
  
  /**
   * Creates a CheckboxInput whose checked state is kept in the provided Var
   * @param value the Var to maintain the checkbox's state
   */
  def apply(value: Var[Boolean] = Var(false))(implicit observing: Observing) = new CheckboxInput(value)
  /**
   * Creates a CheckboxInput with the initial value default. When the value changes it is passes to setter
   * @param default the initial state of the checkbox
   * @param setter the callback to notify of changes
   */
/*  def apply(default: Boolean)(setter: Boolean=>Unit)(implicit observing: Observing) = {
    val v = Var(default)
    v.change foreach setter
    new CheckboxInput(v)
  }
*/}

