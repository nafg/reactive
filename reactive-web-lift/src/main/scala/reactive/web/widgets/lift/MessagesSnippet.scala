package reactive.web.widgets.lift

import scala.xml.NodeSeq
import net.liftweb.http.LiftRules

import reactive.web.widgets.Messages
import reactive.web.lift.AppendToRender

object MessagesSnippet {
  @volatile private var inited = false

  /**
   * Make each [[Messages.defaultMessages]] available as a snippet named `"reactive.Messages"`.
   * The [[Page]] needs to have an [[reactive.web.lift.AppendToRenderTransportType]].
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
