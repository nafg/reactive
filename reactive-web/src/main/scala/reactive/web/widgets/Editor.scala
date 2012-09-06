package reactive
package web
package widgets

import reactive.Signal
import reactive.Observing
import reactive.Val
import reactive.web._
import scala.xml.NodeSeq
import reactive.web.html.Select


case class Editor[A](renderer: NodeSeq => NodeSeq, value: Signal[Validity[A, NodeSeq]], pageIds: PageIds) extends (NodeSeq => NodeSeq) {
  def apply(ns: NodeSeq): NodeSeq = renderer(ns)

  def id(implicit page: Page) = pageIds.id(page)
  /**
   * Returns a new Editor with the value transformed
   */
  def map[B](f: Signal[Validity[A, NodeSeq]] => Signal[Validity[B, NodeSeq]]): Editor[B] = new Editor[B](renderer, f(value), pageIds)
  /**
   * Combine the value with another editor
   * @example {{{
   *  val combined: Editor[(Int, Boolean)] = for {
   *    a <- Editor.value("123")
   *    b <- Editor.value("false")
   *  } yield (a map (_ map (_.toInt)), b map (_ map (_.toBoolean)))
   *  // above desugars to
   *  val combined: Editor[(Int, Boolean)] = Editor.value("123") flatMap { a =>
   *    Editor.value("false") map { b =>
   *      (a map (_ map (_.toInt)), b map (_ map (_.toBoolean)))
   *    }
   *  }
   * }}}
   */
  def flatMap[B](f: Signal[Validity[A, NodeSeq]] => Editor[B]): Editor[B] = {
    val b = f(value)
    new Editor[B](renderer andThen b.renderer, b.value, b.pageIds)
  }

  def <*>[B, C](that: Editor[B])(implicit ev: A <:< (B => C)): Editor[C] = {
    new Editor[C](
      this.renderer andThen that.renderer,
      for {
        va <- this.value
        vb <- that.value
      } yield for {
        a <- va
        b <- vb
      } yield ev(a)(b),
      new PageIds {} // TODO
    )
  }
}

object Editor {
  /**
   * Links an element's `value` attribute/property with a value.
   * @tparam A       the type of the value returned
   * @param in       how to convert from a String to that type
   * @return         a function String=>Editor[A]
   */
  def value(value: String)(implicit observing: Observing, page: Page): Editor[String] = {
    val pv = PropertyVar("value")(value) withEvents DomEventSource.change
    new Editor(pv, pv map { s => Valid(s) }, pv.dom)
  }

  /**
   * Allows to choose a value from a select
   * @return         a function Option[A] => Editor[Option[A]]
   */
  def select[A](choices: Seq[A])(init: Option[A])(implicit observing: Observing, page: Page): Editor[Option[A]] = {
    val s = Select(Val(choices))
    s.selectedItem () = init
    new Editor(s, s.selectedItem map { si => Valid(si) }, s)
  }
}
