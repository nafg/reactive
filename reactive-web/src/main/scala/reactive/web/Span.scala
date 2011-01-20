package reactive
package web

import scala.xml.NodeSeq

import net.liftweb.http.js.JsCmds.SetHtml


class Span(content: Signal[NodeSeq] = Val(NodeSeq.Empty)) extends RElem {
  def baseElem = <span>{content.now}</span>
  def events = Nil
  def properties = Nil
  override def addPage(implicit page: Page) {
    super.addPage(page)
    content foreach {s =>
      Reactions.inAnyScope(page) {
        Reactions.queue(SetHtml(id, s))
      }
    }
  }
  override def toString = "Span(" + render + ")"
}

object Span {
  def apply(content: Signal[NodeSeq] = Val(NodeSeq.Empty)) = new Span(content)
  
  def apply(binding: Signal[NodeSeq=>NodeSeq]) = {ns: NodeSeq =>
    new Span(binding map (_(ns))).render
  }  
}
