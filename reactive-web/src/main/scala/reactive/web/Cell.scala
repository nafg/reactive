package reactive
package web


import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.js.JsCmds.SetHtml


trait Cell extends RElem {
  def content: Signal[NodeSeq]
  
  override def render = super.render.copy(child = content.now)
  
  override def addPage(implicit page: Page) {
    super.addPage(page)
    content foreach {s =>
      Reactions.inAnyScope(page) {
        Reactions.queue(SetHtml(id, s))
      }
    }
  }
}


object Cell {
  def apply(binding: Signal[NodeSeq=>NodeSeq]) = {ns: NodeSeq =>
    new Cell {
      val events, properties = Nil
      val baseElem = nodeSeqToElem(ns)
      lazy val content = binding map {_(ns)}
    }.render
  }
}
