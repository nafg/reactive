package reactive
package web
package widgets

import scala.xml._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.LiftRules
import net.liftweb.http.RequestVar

/**
 * The Messages companion object provides a default template.
 * In addition, it is itself a `RequestVar[Messages]`, so
 * you can use it as a global request-scoped `Messages`.
 * To install it as a snippet call `init`.
 */
object Messages extends RequestVar(new Messages) {
  /**
   * A default template for the Messages widget
   * @param messageClass Text to append to the class attribute of each message.
   *                     If you need different messages to have different classes
   *                     (e.g., warning vs. notice) you can use multiple `Messages`
   *                     instances, each with a different template `messageClass`.
   */
  def template(messageClass: String = "") =
    <div id="messages">
      <div class={ "message "+messageClass }>
        <a href="javascript://" class="close">×</a>
        <span class="text"></span>
      </div>
    </div>

  @volatile private var inited = false
  /**
   * Make the global Messages RequestVar available as a snippet, `reactive.Messages`.
   * @param tmplt The template to use. Defaults to `template`.
   */
  def init(tmplt: NodeSeq = template()) = {
    assert(!inited, "Cannot initialize twice!")
    inited = true
    LiftRules.snippets.append {
      case "reactive" :: "Messages" :: Nil => is.renderWithTemplate(tmplt)
    }
  }
}

/**
 * A widget to display modeless pop up messages, optionally with a close link.
 */
class Messages {
  val messages = BufferSignal[NodeSeq]()
  def apply() = messages.value
  def update(ms: Seq[NodeSeq]) = messages.value = ms
  def update(ms: NodeSeq) = messages.value = ms :: Nil
  def update(ms: String) = messages.value = Text(ms) :: Nil
  def +=(m: NodeSeq) = messages.value :+= m
  def +=(s: String) = messages.value :+= Text(s)
  def -=(m: NodeSeq) = messages.value -= m
  def -=(s: String) = messages.value -= Text(s)

  /**
   * Returns a Repeater that displays the messages
   */
  def render(implicit page: Page) = Repeater(
    messages.now map { m =>
      ".text *" #> m &
        ".close" #> onServer[Click]{ _ => messages.value -= m }
    } signal
  )
  /**
   * Render the Repeater with a template
   */
  def renderWithTemplate(template: NodeSeq = Messages.template())(implicit page: Page): NodeSeq => NodeSeq = { ns =>
    render apply template
  }
}
