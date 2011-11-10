package reactive

import scala.xml.{ Elem, Group, Node, NodeSeq }
import net.liftweb.http.{ js, SHtml, S }
import js.JE.{ JsRaw, Str }
import js.JsCmds

/**
 * reactive-web package
 */
package object web {
  object packageLogger extends Logger {
    case class WrappedNonElemInSpan(xml: NodeSeq) extends LogEventPredicate
  }

  @deprecated("Use DomEventSource")
  val DOMEventSource = DomEventSource
  @deprecated("Use DomEventSource")
  type DOMEventSource[T <: DomEvent] = DomEventSource[T]
  @deprecated("Use DomProperty")
  val DOMProperty = DomProperty
  @deprecated("Use DomProperty")
  type DOMProperty = DomProperty
  @deprecated("User DomEvent")
  type DOMEvent = DomEvent

  /**
   * Queues a javascript confirm dialog. The user's response is passed to the
   * provided PartialFunction.
   * Requires a Page to be in the implicit scope or to be passed in manually. However
   * it is only used if there is no current reactions scope.
   * @param message The message to display in the confirm dialog box
   * @param response a PartialFunction[Boolean, Unit] used to handle the user's response to the dialog box
   */
  //TODO should we optimize so ajax call is only made for response.isDefinedAt case?
  //  down side is that a nondeterministic PF (e.g. {case _ if random>.5 => } etc.) won't work
  def confirm(message: String)(response: PartialFunction[Boolean, Unit])(implicit page: Page): Unit = {
    val funcWrapper: String => Unit = {
      case "true"  => if (response.isDefinedAt(true)) response(true)
      case "false" => if (response.isDefinedAt(false)) response(false)
    }
    val js = S.fmapFunc(S.contextFuncBuilder(RElem.ajaxFunc(funcWrapper))) { funcId =>
      SHtml.makeAjaxCall(
        JsRaw("'"+funcId+"='+confirm("+Str(message).toJsCmd+")")
      ).toJsCmd
    }
    Reactions.inAnyScope(page){
      Reactions queue JsCmds.Run(js)
    }
  }
  /**
   * Queues a JavaScript alert dialog in the current scope, or, if none
   * exist, in the server scope of the implicit page parameter.
   * @param message the text to display
   */
  def alert(message: String)(implicit page: Page) {
    Reactions.inAnyScope(page){
      Reactions queue JsCmds.Run("alert("+Str(message).toJsCmd+")")
    }
  }

  private[web] def trimNodeSeq(ns: NodeSeq): NodeSeq = {
    def emptyText: Node => Boolean = {
      case scala.xml.Text(s) if s.trim.isEmpty => true
      case _                                   => false
    }
    ns.dropWhile(emptyText).reverse.dropWhile(emptyText).reverse
  }

  /**
   * Force a NodeSeq to an Elem. If necessary adds
   * a surrounding &lt;span&gt; tag.
   */
  //TODO should we not be trimming?
  def nodeSeqToElem(ns: NodeSeq): Elem = trimNodeSeq(ns) match {
    case e: Elem                       => e
    case Seq(e: Elem)                  => e
    case scala.xml.Text(s)             => <span>{ s.trim }</span>
    case Seq(node: Node) if node ne ns => nodeSeqToElem(node)
    case xml =>
      //TODO should we just throw an exception?
      packageLogger warn packageLogger.WrappedNonElemInSpan(xml)
      <span>{ xml }</span>
  }

  private[web] def bindFunc2contentFunc[T](bindFunc: Signal[NodeSeq => NodeSeq])(andThen: Signal[NodeSeq] => T): NodeSeq => T =
    ns => andThen(bindFunc map (_(ns)))

  /**
   * [T](SeqSignal[NodeSeq=>NodeSeq])(SeqSignal[NodeSeq]=>T)(NodeSeq=>T)
   */
  private[web] def bindFunc2seqContentFunc[T](bindFunc: SeqSignal[NodeSeq => NodeSeq])(andThen: SeqSignal[NodeSeq] => T): NodeSeq => T =
    ns => andThen(bindFunc map { _ map { _(ns) } })

  /**
   * Given a Class instance, extract the original scala identifier name.
   * Class names can be of the form [[abc] $] name [$ [nnn] ...]
   */
  private[web] def scalaClassName(c: Class[_]) = {
    val name = c.getSimpleName
    val dropEnd = name.replaceAll("""(\$\d*)*\z""", "")
    dropEnd.toList.reverse.takeWhile('$'!=).reverse.mkString
  }
}
