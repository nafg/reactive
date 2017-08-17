package reactive.web.html

import reactive.Observing
import reactive.web.DomEventSource
import reactive.web.Page
import reactive.web.PropertyCodec
import reactive.web.PropertyVar
import reactive.web.CanRenderDomMutationConfig
import scala.xml.Elem
import scala.xml.Text
import reactive.web.ElemFuncWrapper
import scala.xml.NodeSeq
import reactive.web.Cell

class TextArea(initial: String)(implicit observing: Observing, defaultCodec: PropertyCodec[String], config: CanRenderDomMutationConfig) {
  private val textareaCodec = new PropertyCodec[String] {
    def fromString = defaultCodec.fromString
    val toJS = defaultCodec.toJS
    def toAttributeValue = _ => _ => None
  }

  val value = PropertyVar.value(initial)(textareaCodec, implicitly, implicitly) withEvents DomEventSource.change

  private def cell(implicit page: Page) = Cell(value map { v => _: NodeSeq => Text(v) })


  def render(implicit page: Page): NodeSeq => NodeSeq = value andThen cell
}

object TextArea {
  def apply(initial: String = "")(implicit observing: Observing, defaultCodec: PropertyCodec[String], config: CanRenderDomMutationConfig) =
    new TextArea(initial)(observing, defaultCodec, config)

  implicit class toNodeSeqFunc(textArea: TextArea)(implicit page: Page) extends (NodeSeq => NodeSeq) {
    def apply(ns: NodeSeq) = textArea.render(page)(ns)
  }
}
