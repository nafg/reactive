package reactive
package web

import net.liftweb.http.S
import net.liftweb.http.js.{ JsCmd, JsCmds }
import JsCmds.Run

/**
 * A Scope represents a dynamic scope in which javascript is queued and collected.
 * It is up to the Scope what to do with queued javascript
 */
trait Scope {
  def queue[T: CanRender](renderable: T)
}
/**
 * A Scope that sends queued javascript to a Page's ReactionsComet.
 */
case class CometScope(page: Page) extends Scope {
  def queue[T](renderable: T)(implicit canRender: CanRender[T]) {
    page.comet queue renderable
  }
}
/**
 * A scope that stores queued javascript
 * (used during ajax calls, to return the
 * queued javascript as the ajax response)
 */
class LocalScope extends Scope {
  var js: List[JsCmd] = Nil
  def queue[T](renderable: T)(implicit canRender: CanRender[T]) = {
    val s = canRender(renderable)
    js :+= Run(s)
  }
  def dequeue: JsCmd = {
    val ret = js.head
    js = js.tail
    ret
  }
}
/**
 * A scope that calls S.appendJs with
 * queued javascript. Allows one to queue javascript
 * even during initial page render.
 */
case object DefaultScope extends Scope {
  def queue[T](renderable: T)(implicit canRender: CanRender[T]) = S.appendGlobalJs(Run(canRender(renderable)))
}
