package reactive
package web

/**
 * Typeclass for types that can be rendered as javascript and sent to the browser
 */
trait CanRender[-T] {
  def apply(renderable: T): Renderable
}

object CanRender {
  def apply[T](f: T => Renderable) = new CanRender[T] {
    def apply(renderable: T) = f(renderable)
  }

  implicit val string: CanRender[String] = CanRender(StringRenderable)

  implicit val renderable: CanRender[Renderable] = CanRender(identity)
}

/**
 * Base trait for things that can be sent to the browser
 */
trait Renderable {
  def render: String
}

case class StringRenderable(string: String) extends Renderable {
  def render = string
}
