package reactive
package web
package widgets

import scala.xml._
import net.liftweb.util._
import net.liftweb.util.Helpers._

/**
 * The Messages companion object provides a default template.
 * In addition it provides a default per-page instance.
 */
object Messages {
  private object counter extends IdCounter

  /**
   * A default template for the Messages widget
   * @param messageClass Text to append to the class attribute of each message.
   *                     If you need different messages to have different classes
   *                     (e.g., warning vs. notice) you can use multiple `Messages`
   *                     instances, each with a different template `messageClass`.
   */
  def template(messageClass: String = "") =
    <div id={ "messages" + counter.nextNumber } class="messages-outer">
      <div class={ "message "+messageClass }>
        <a href="javascript://" class="close">×</a>
        <span class="text"></span>
      </div>
      <style type="text/css" data-lift="head">{ Text("""
        .messages-outer {
          position: fixed;
          top: 50px;
          left: 3em;
          width: auto;
          max-width: 90%;
          min-width: 10%;
          text-align: left;
        }

        .message {
          position: relative;
          background: rgba(246, 225, 128, 0.85);
          border: 3px solid rgba(255, 128, 0, 0.25);
          border-radius: 10px;
          margin: .25em;
          padding: .5em 3em .5em 2em;
          font-weight: bold;
          box-shadow: 1px 1px 5px rgb(192, 192, 192);
        }

        .message .text {
          text-shadow: 1px 1px 0.5px white;
          font-family: sans-serif;
        }

        .message .close {
          display: block;
          position: absolute;
          top: 2px;
          right: 5px;
          text-decoration: none;
          font-size: small;
          padding: 0px 4px;
          border-radius: 4px;
          box-shadow: 0px 0px 3px rgb(255, 255, 235);
          cursor: pointer;
        }
      """) }</style>
    </div>

  private val _defaultMessages = scala.collection.mutable.WeakHashMap[Page, Messages]()

  /**
   * A default per-[[Page]] instance
   */
  def defaultMessages(implicit page: Page) = _defaultMessages.getOrElseUpdate(page, new Messages)

  implicit def singletonToDefaultMessages(m: Messages.type)(implicit page: Page): Messages = m.defaultMessages
}

/**
 * A widget to display modeless pop up messages, optionally with a close link.
 */
class Messages {
  implicit object observing extends Observing
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
   * Returns `NodeSeq => NodeSeq` that renders a Repeater that displays the messages
   */
  def render(implicit page: Page): NodeSeq => NodeSeq = Repeater(
    messages.now.map{ m =>
      ".text *" #> m &
        ".close" #> onServer[Click]{ _ => messages.value -= m }
    }.signal
  )
  /**
   * Render the Repeater with a template
   */
  def renderWithTemplate(template: NodeSeq = Messages.template())(implicit page: Page): NodeSeq =
    render apply template
}
