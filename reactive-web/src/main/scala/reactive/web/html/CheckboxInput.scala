package reactive
package web
package html


/**
 * Represents a checkbox input in the DOM
 * @param _value a Var that represents the value of the checkbox
 */
class CheckboxInput(
  _value: Var[Boolean] = Var(false)
) extends RElem {
  /**
   * The dblclick DOM event
   */
  //TODO move common events into traits
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
   * The checked property. Whether the checkbox is checked.
   */
  val checked = CheckboxInput.checked(_value)
  checked updateOn change
  
  def events = List(dblClick, keyUp, change)
  def properties = List(checked)
  def baseElem = <input type="checkbox"/>
}

/**
 * Provides several factories for creating CheckboxInputs
 */
object CheckboxInput {
  def checked(v: Var[Boolean]) = new DOMProperty[Boolean] {
    val value = v
    def fromString(s: String) = s.toLowerCase match {
      case "" | net.liftweb.util.Helpers.AsInt(0) | "false" => false
      case _ => true
    }
    def asString(v: Boolean) = if(v) "true" else "false"
    override def asAttribute = if(value.now) new scala.xml.UnprefixedAttribute(name, "checked", scala.xml.Null) else scala.xml.Null
    
    def name = "checked"
  }
  /**
   * Creates a CheckboxInput whose checked state is kept in the provided Var
   * @param value the Var to maintain the checkbox's state
   */
  def apply(value: Var[Boolean] = Var(false)) = new CheckboxInput(value)
  /**
   * Creates a CheckboxInput with the initial value default. When the value changes it is passes to setter
   * @param default the initial state of the checkbox
   * @param setter the callback to notify of changes
   */
  def apply(default: Boolean)(setter: Boolean=>Unit)(implicit observing: Observing) = {
    val v = Var(default)
    v.change foreach setter
    new CheckboxInput(v)
  }
}

