package reactive.web
package widgets

import scala.xml.NodeSeq
import net.liftweb.http.LiftRules

object MessagesSnippet {
  @volatile private var inited = false

  /**
   * Make each [[Page]]'s default Messages instance available as a snippet, `reactive.Messages`.
   * @param tmplt The template to use. Defaults to `Messages.template()`.
   */
  def init(tmplt: NodeSeq = Messages.template()) = {
    assert(!inited, "Cannot initialize twice!")
    inited = true
    LiftRules.snippets.append {
      case "reactive" :: "Messages" :: Nil =>
        { _: NodeSeq =>
          AppendToRender.currentPages.flatMap{ implicit page =>
            Messages.defaultMessages.renderWithTemplate(tmplt)
          }
        }
    }
  }
}
