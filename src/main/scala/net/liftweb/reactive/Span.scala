package net.liftweb.reactive

import _root_.reactive._
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
  override def toString = "TextSpan(" + render + ")"
}
object Span {
  def apply(content: Signal[NodeSeq] = Val(NodeSeq.Empty)) = new Span(content)
}
