package reactive

import scala.xml.{Elem, Group, Node, NodeSeq}

import net.liftweb.http.{js, SHtml, S}
  import js.JE.{JsRaw, Str}
  import js.JsCmds

  
/**
 * reactive-web package
 */
package object web {
  /**
   * Queues a javascript confirm dialog. The user's response is passed to the
   * provided PartialFunction.
   * Requires a Page to be in the implicit scope or to be passed in manually. However
   * it is only used if there is no current reactions scope.
   * @param message The message to display in the confirm dialog box
   * @param response a PartialFunction[Boolean, Unit] used to handle the user's response to the dialog box
   */
  def confirm(message: String)(response: PartialFunction[Boolean, Unit])(implicit page: Page): Unit = {
    val funcWrapper: String=>Unit = {
      case "true" => if(response.isDefinedAt(true)) response(true)
      case "false" => if(response.isDefinedAt(false)) response(false)
    }
    val js = S.fmapFunc(S.contextFuncBuilder(RElem.ajaxFunc(funcWrapper))) {funcId =>
      SHtml.makeAjaxCall(
        JsRaw("'"+funcId+"='+confirm(" + Str(message).toJsCmd + ")")
      ).toJsCmd
    }
    Reactions.inAnyScope(page)(
      Reactions.queue(
        JsCmds.Run(js)
      )
    )
  }
  
  //TODO logging
  private[web] def nodeSeqToElem(ns: NodeSeq): Elem = ns match {
    case e: Elem => e
    case Group(nodes) if nodes.length==1 => nodeSeqToElem(nodes(0))
    case nodes: Seq[Node] if nodes.length==1 => nodeSeqToElem(nodes(0))
    case other =>
      println("Warning: NodeSeq is not an Elem; wrapping it in a span. "+other.getClass+": "+other.toString)
      <span>{other}</span>
  }
}
