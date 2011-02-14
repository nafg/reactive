package reactive
package web


import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.js.JsCmds.SetHtml


/**
 * A Cell is an RElem whose contents are determined by a simple Signal.
 * It is the single-child counterpart of Repeater.
 */
trait Cell extends RElem {
  /**
   * The Signal that determines the contents of the element.
   */
  def content: Signal[NodeSeq]
  
  override def render(implicit p: Page) = super.render.copy(child = content.now)
  
  override def addPage(implicit page: Page) {
    super.addPage(page)
    content foreach {s =>
      Reactions.inAnyScope(page) {
        Reactions.queue(SetHtml(id, s))
      }
    }
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
  def apply(binding: Signal[NodeSeq=>NodeSeq])(implicit p: Page) = {ns: NodeSeq =>
    new Cell {
      val events, properties = Nil
      val baseElem = nodeSeqToElem(ns)
      lazy val content = binding map {_(baseElem.child)}
    }.render
  }
}
