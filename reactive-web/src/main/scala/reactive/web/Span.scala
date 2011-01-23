package reactive
package web

import scala.xml.NodeSeq


trait Span extends RElem {
  def baseElem = <span/>
  def events = Nil
  def properties = Nil
  
  override def toString = "Span(" + render + ")"
}

object Span {
  def apply(content: Signal[NodeSeq] = Val(NodeSeq.Empty)) = {
    val tmp = content
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

