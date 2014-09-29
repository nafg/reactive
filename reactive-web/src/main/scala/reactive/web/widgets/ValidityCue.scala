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
     * The css selector for the element to which classes for the validity states should be added
     */
    def validitySelector = ".val"
    /**
     * The css selector for the element to which validity messagese added
     */
    def messageSelector = ".msg"
    /**
     * The css selector for the element to which the [[Editor]] should be rendered
     */
    def editorSelector = ".edit"
    def validClass: Option[String] = None
    def invalidClass = "error"
    def warningClass = "warning"
    def applyClasses[A](v: Signal[Validity[A, NodeSeq]])(implicit page: Page, obs: Observing): NodeSeq => NodeSeq = {
      val classes = v map {
        case _: Valid[_]      => (true, false, false)
        case _: Warning[_, _] => (false, true, false)
        case _: Invalid[_]    => (false, false, true)
      }
      validClass.fold(identity[NodeSeq] _)(c => PropertyVar.toggleClass(c)(classes.map(_._1))) andThen
        PropertyVar.toggleClass(warningClass)(classes.map(_._2)) andThen
        PropertyVar.toggleClass(invalidClass)(classes.map(_._3))
    }
    def message[A](v: Validity[A, NodeSeq]): NodeSeq => NodeSeq = v match {
      case Valid(_) => _ => NodeSeq.Empty
      case v        => _ => v.messages.map(ns => <p>{ ns }</p>)
    }
  }
  def validityCue[A](editor: Editor[A])(implicit observing: Observing, page: Page, cueConfig: ValidityCueConfig, rdmConfig: CanRenderDomMutationConfig): Editor[A] = {
    import cueConfig._
    new Editor(
      validitySelector #> applyClasses(editor.value) andThen
        messageSelector #> reactive.web.Cell(editor.value map message) andThen
        editorSelector #> editor,
      editor.value,
      editor.pageIds
    )
  }
}
