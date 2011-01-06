package reactive
package web



class Div(
  override val children: SeqSignal[RElem]
) extends RParentElem {
  //println(page.id + ": created Div " + id)
  val dblClick = new JSEventSource[DblClick]
  val keyUp = new JSEventSource[KeyUp]
      
  def events = List(dblClick, keyUp)
  def properties = Nil
  def baseElem = <div />
  
  override def render = {
    val ret = super.render
    //println(page.id + ": rendering " + ret)
    ret
  }
}

