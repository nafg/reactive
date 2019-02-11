package reactive
package routing
package lift

import scala.xml.NodeSeq

import net.liftweb.http.S
import net.liftweb.util.Helpers.StringToCssBindPromoter

/**
 * An [[HtmlResponse]] that loads the template from a file
 * @param path the template path (use '/' to separate path components)
 * @param snippets the named scala functions referenced in the template using `data-lift` etc.
 */
class Template(val path: String, val snippets: Snippet*) extends HtmlResponse {
  /**
   * Loads the template from [[path]] via `S.runTemplate`, using
   * the specified `snippets`.
   */
  def template = S.runTemplate((path split "/").toList, snippets.map(_.asTuple): _*)
}
object Template {
  def apply(path: String, snippets: Snippet*) = new Template(path, snippets: _*)
}

case class Snippet(name: String, render: NodeSeq => NodeSeq) {
  def asTuple = (name, render)
}
object Snippet {
  /**
   * @param func Snippet function. Call by name.
   */
  def apply(name: String)(func: => NodeSeq => NodeSeq): Snippet = Snippet(name, { ns: NodeSeq => func(ns) })

  /**
   * Creates a [[Snippet]] named `"Title"` that appends `text` to
   * an element's content.
   */
  def title(text: String) = Snippet("Title", "* *+" #> text)
}
