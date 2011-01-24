package reactive
package web

import scala.xml.NodeSeq


trait Span extends RElem {
  val baseElem = <span/>
  val events = Nil
  val properties = Nil
  
  override def toString = "Span(" + render + ")"
}

object Span {
  def apply(content: Signal[NodeSeq] = Val(NodeSeq.Empty)) = {
    def tmp = content
    new Span with Cell {
      def content = tmp
    }
  }
  
  def apply(binding: Signal[NodeSeq=>NodeSeq]) = {ns: NodeSeq =>
    new Span with Cell {
      lazy val content = binding map (_(ns))
    }.render
  }  
}

