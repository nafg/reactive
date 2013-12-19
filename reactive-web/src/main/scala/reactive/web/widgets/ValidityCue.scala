package reactive
package web
package widgets

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._

/**
 * Allows to wrap an Editor, returning an Editor, that adds a visual of the validity state. 
 */
object ValidityCue {
  class ValidityCueConfig {
    /**
     * The css selector for the element to add a class for the validity state
     */
    def validitySelector = ".val"
    def messageSelector = ".msg"
    def editorSelector = ".edit"
    def validClass: Option[String] = None
    def invalidClass = "error"
    def warningClass = "warning"
    def validityClass[A](v: Validity[A, NodeSeq]): Option[String] = v match {
      case _: Valid[_]      => validClass
      case _: Warning[_, _] => Some(warningClass)
      case _: Invalid[_]    => Some(invalidClass)
    }
    def message[A](v: Validity[A, NodeSeq]): NodeSeq => NodeSeq = v match {
      case Valid(_) => _ => NodeSeq.Empty
      case v        => _ => v.messages.map(ns => <p>{ ns }</p>)
    }
  }
  def validityCue[A](editor: Editor[A])(implicit observing: Observing, page: Page, config: ValidityCueConfig): Editor[A] = {
    import config._
    new Editor(
      validitySelector #> PropertyVar.appendClass(editor.value map validityClass) andThen
        messageSelector #> reactive.web.Cell(editor.value map message) andThen
        editorSelector #> editor,
      editor.value,
      editor.pageIds
    )
  }
}
