package reactive
package web

import net.liftweb.http.S
import net.liftweb.http.js.{ JsCmd, JsCmds }
import JsCmds.Run
import scala.xml.NodeSeq

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
    page.comet queue Run(canRender(renderable))
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
  def replace(f: JsCmd => JsCmd): Unit = queue(f(dequeue))
}
/**
 * A scope that calls S.appendJs with
 * queued javascript. Allows one to queue javascript
 * even during initial page render.
 */
case object DefaultScope extends Scope {
  def queue[T](renderable: T)(implicit canRender: CanRender[T]) = S.appendJs(Run(canRender(renderable)))
}

/**
 * A scope to simulate the dom mutations that the browser would apply,
 * by directly applying transformations to a NodeSeq
 * @param init the initial NodeSeq (such as a template)
 */
class TestScope(init: NodeSeq) extends LocalScope {
  /**
   * The current xml
   */
  var xml = init
  override def queue[T: CanRender](renderable: T): Unit = {
    renderable match {
      case dm: DomMutation =>
        xml = dm(xml)
      case _ =>
    }
    super.queue(renderable)
  }
}
