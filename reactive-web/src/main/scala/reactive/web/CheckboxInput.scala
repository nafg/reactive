package reactive
package web



class CheckboxInput(
  _value: Var[Boolean] = Var(false)
) extends RElem {
  
  val dblClick = new JSEventSource[DblClick]
  val keyUp = new JSEventSource[KeyUp]
  val change = new JSEventSource[Change.type]
  val checked = new JSProperty[Boolean] {
    val value = _value
    def fromString(s: String) = s.toLowerCase match {
      case "" | net.liftweb.util.Helpers.AsInt(0) | "false" => false
      case _ => true
    }
    def asString(v: Boolean) = if(v) "true" else "false"
    override def asAttribute = if(value.now) new scala.xml.UnprefixedAttribute(name, "checked", scala.xml.Null) else scala.xml.Null
    
    def name = "checked"
    def elemId = id
    this updateOn change
  }
  
  def events = List(dblClick, keyUp, change)
  def properties = List(checked)
  def baseElem = <input type="checkbox"/>

}
object CheckboxInput {
  def apply(value: Var[Boolean] = Var(false)) = new CheckboxInput(value)
  def apply(default: Boolean)(setter: Boolean=>Unit)(implicit observing: Observing) = {
    val v = Var(default)
    v.change foreach setter
    new CheckboxInput(v)
  }
}

