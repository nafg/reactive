package reactive
package web

import scala.xml.NodeSeq
import net.liftweb.http.S
import scala.xml.Group

object LiftDomMutationRenderer extends DomMutationRenderer {
  def processHtml(uid: String, content: NodeSeq) = S.session.
    map(s =>
      s.fixHtml(s.processSurroundAndInclude("JS SetHTML id: "
        + uid,
        content))).
    openOr(content)

  def renderHtml(html: NodeSeq) = {
    val writer = new java.io.StringWriter
    S.htmlProperties.htmlWriter(Group(html), writer)
    writer.toString
  }
}
