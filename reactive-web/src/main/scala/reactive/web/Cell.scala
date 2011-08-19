package reactive
package web

import scala.xml.{ Elem, NodeSeq }
import net.liftweb.http.js.JsCmds.SetHtml

/**
 * A Cell is an RElem whose contents are determined by a simple Signal.
 * It is the single-child counterpart of Repeater.
 */
trait Cell extends RElem {
  implicit def renderer: CanRender[DomMutation]
  /**
   * The Signal that determines the contents of the element.
   */
  def content: Signal[NodeSeq]

  override def renderer(implicit p: Page): Elem => Elem = e => super.renderer(p)(e).copy(child = content.now)

  override def addPage(elem: Elem)(implicit page: Page): Elem = {
    val ret = super.addPage(elem)(page)
    content.change foreach { s =>
      Reactions.inAnyScope(page) {
        Reactions.queue(DomMutation.ReplaceAll(id, s))
      }
    }
    ret
  }
}

/**
 * Provides a factory for creating Cells.
 */
object Cell {
  /**
   * Returns a function that can be used in Lift binding.
   * It will use the element being bound from the template as the parent element,
   * and it will use the function value of the Signal to bind its contents.
   * The Signal's value will be passed the children of the element used as the parent.
   * @param binding the binding-function-valued Signal
   */
  def apply(binding: Signal[NodeSeq => NodeSeq])(implicit p: Page, config: CanRenderDomMutationConfig): NodeSeq => NodeSeq = { ns: NodeSeq =>
    new Cell {
      def renderer = config.domMutationRenderer
      val events, properties = Nil
      val baseElem = nodeSeqToElem(ns)
      lazy val content = binding map { _(baseElem.child) }
    }.render
  }
}
