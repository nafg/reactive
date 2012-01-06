package reactive
package web
package widget

import scala.xml._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.LiftRules
import net.liftweb.http.RequestVar

object Messages extends RequestVar(new Messages) {
  /**
   * A default template for the Messages widget
   */
  def template =
    <div id="messages">
      <div class="message">
        <a href="javascript://" class="close">X</a><span class="text"></span>
      </div>
    </div>

  def init = LiftRules.snippets.append {
    case "reactive" :: "Messages" :: Nil => is.renderWithTemplate()
  }
}

/**
 * A widget to display modeless pop up messages with a close button.
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
  def render(implicit page: Page) = Repeater(messages map (_ map { m =>
    ".text *" #> m &
      ".close" #> (DomEventSource.click ->> { messages.value -= m })
  }))
  /**
   * Render the template with the Repeater
   */
  def renderWithTemplate(template: NodeSeq = Messages.template)(implicit page: Page): NodeSeq => NodeSeq = { ns =>
    render apply nodeSeqToElem(ns).copy(child = template)
  }
}
