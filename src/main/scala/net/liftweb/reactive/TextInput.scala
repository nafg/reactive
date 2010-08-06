package net.liftweb.reactive

import _root_.reactive._


class TextInput(
  _value: Var[String] = Var("")
) extends RElem {
  
  val dblClick = new JSEventSource[DblClick]
  val keyUp = new JSEventSource[KeyUp]
  val change = new JSEventSource[Change.type]
  val value = new JSStringProperty {
    def name = "value"
    def elemId = id
    this updateOn change
    //value ()= _value.now
    val value = _value
  }
  
  def events = List(dblClick, keyUp, change)
  def properties = List(value)
  def baseElem = <input type="text" />
}

object TextInput {
  def apply(value: Var[String] = Var("")) = new TextInput(value)
  def apply(default: String)(setter: String=>Unit)(implicit observing: Observing) = {
    val v = Var(default)
    v.change foreach setter
    new TextInput(v)
  }
}

