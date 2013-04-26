package reactive

import scala.xml.{ Elem, Group, Node, NodeSeq }
import net.liftweb.http.{ js, SHtml, S }
import js.JE.{ JsRaw, Str }
import js.JsCmds
import web.javascript.{ JsExp, JsTypes, =|>, Javascript, window, Ajax }

/**
 * reactive-web package
 */
package object web {
  object packageLogger extends Logger {
    case class WrappedNonElemInSpan(xml: NodeSeq)
  }

  @deprecated("Use DomEventSource", "0.2")
  val DOMEventSource = DomEventSource
  @deprecated("Use DomEventSource", "0.2")
  type DOMEventSource[T <: DomEvent] = DomEventSource[T]
  @deprecated("Use DomProperty", "0.2")
  val DOMProperty = DomProperty
  @deprecated("Use DomProperty", "0.2")
  type DOMProperty = DomProperty
  @deprecated("Use DomEvent", "0.2")
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
  def confirm(message: String)(response: PartialFunction[Boolean, Unit])(implicit page: Page, observing: Observing): Unit = Javascript {
    Ajax { response.orElse[Boolean, Unit] { case _ => } } apply window.confirm(message)
  }

  /**
   * Queues a JavaScript alert dialog in the current scope, or, if none
   * exist, in the server scope of the implicit page parameter.
   * @param message the text to display
   */
  def alert(message: String)(implicit page: Page) = javascript.Javascript {
    window.alert(message)
  }

  /**
   * Add a javascript event handler
   * @tparam e the event type (must be written explicitly)
   * @param f the javascript function expression (can be implicity converted via the javascript dsl; see example)
   * @return a DomEventSource
   * @example
   * {{{
   *   on[Click]{ x: JsExp[JsObj] =>
   *     Return(false)
   *   }
   * }}}
   */
  def on[E <: DomEvent](f: JsExp[JsTypes.JsObj =|> JsTypes.JsVoid])(implicit m: Manifest[E], ee: EventEncoder[E], o: Observing, p: Page) = {
    val des = new DomEventSource[E]
    des.jsEventStream(p) foreach f
    des
  }
  /**
   * Add a server (ajax) event handler
   * @tparam e the event type (note that this can be inferred from the type of ''f'')
   * @param f the function that handles the event
   * @return a DomEventSource
   * @example
   * {{{ onServer { _: Click => save() } }}}
   */
  def onServer[E <: DomEvent](f: E => Unit)(implicit m: Manifest[E], ee: EventEncoder[E], o: Observing, p: Page) = {
    val des = new DomEventSource[E]
    des.eventStream(p) foreach f
    des
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
    ns => andThen(bindFunc.now.map(_(ns)).signal)

  /**
   * Given a Class instance, extract the original scala identifier name.
   * Class names can be of the form [[abc] $] name [$ [nnn] ...]
   */
  private[web] def scalaClassName(c: Class[_]) = {
    val name = c.getSimpleName
    val dropEnd = name.replaceAll("""(\$\d*)*\z""", "")
    dropEnd.toList.reverse.takeWhile('$' != _).reverse.mkString
  }

//  implicit def toForwardable[A : CanForwardFrom]
}
