package reactive
package web


import scala.xml.NodeSeq

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
