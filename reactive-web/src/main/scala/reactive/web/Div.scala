package reactive
package web



trait Div extends RElem {
  lazy val dblClick = new JSEventSource[DblClick]
  lazy val keyUp = new JSEventSource[KeyUp]
      
  def events = List(dblClick, keyUp)
  def properties = Nil
  def baseElem = <div />
}

object Div {
  import scala.xml.NodeSeq
  
  def apply(content: SeqSignal[RElem]) = new Div with Repeater {
    def children = content
  }
  
  def apply(binding: SeqSignal[_ <: NodeSeq=>NodeSeq]) = {ns: NodeSeq =>
    new Div with Repeater {
      lazy val children = binding map {
        _ map {f => RElem(<span>{f(ns)}</span>)}
      }
    }.render
  }
}
