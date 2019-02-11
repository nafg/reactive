package reactive.web.widgets.lift

import scala.xml.NodeSeq

import net.liftweb.http.LiftRules
import reactive.web.CanRenderDomMutationConfig
import reactive.web.lift.AppendToRender
import reactive.web.widgets.Messages

object MessagesSnippet {
  @volatile private var inited = false

  /**
   * Make each [[Messages.defaultMessages]] available as a snippet named `"reactive.Messages"`.
   * The [[reactive.web.Page]] needs to have an [[reactive.web.lift.AppendToRenderTransportType]].
   *
   * @param template The template to use. Defaults to `Messages.template()`.
   */
  def init(template: NodeSeq = Messages.template())(implicit rdmCfg: CanRenderDomMutationConfig) = {
    assert(!inited, "Cannot initialize twice!")
    inited = true
    LiftRules.snippets.append {
      case "reactive" :: "Messages" :: Nil =>
        _: NodeSeq =>
          AppendToRender.currentPages.flatMap{ implicit page =>
            Messages.defaultMessages.renderWithTemplate(template)
          }
    }
  }
}
