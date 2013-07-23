package reactive
package web

/**
 * Typeclass for types that can be rendered as javascript and sent to the browser
 */
trait CanRender[-T] {
  def apply(renderable: T): String
}

object CanRender {
  def apply[T](f: T => String) = new CanRender[T] {
    def apply(renderable: T) = f(renderable)
  }

  implicit val string: CanRender[String] = CanRender(identity)
  implicit val jsStatement: CanRender[javascript.JsStatement] = CanRender(javascript.JsStatement.render)

  implicit def domMutation(implicit config: CanRenderDomMutationConfig) = config.domMutationRenderer
}
