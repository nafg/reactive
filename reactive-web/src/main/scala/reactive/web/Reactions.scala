package reactive
package web

import scala.collection.mutable.{ WeakHashMap, HashMap }
import scala.ref.WeakReference
import scala.xml.NodeSeq

import net.liftweb.actor.LiftActor
import net.liftweb.http.{ js, S, CometActor, CometCreationInfo, LiftSession, LiftRules }
import js.{ JsCmd, JsCmds }
import JsCmds._
import net.liftweb.common.{ Box, Full }
import net.liftweb.util.{ Helpers, ThreadGlobal }

/**
 * This singleton is used to enable reactive-web's features and to send javascript to the browser
 */
object Reactions extends Logger {
  case class QueueingJS[T: CanRender](pageId: Option[String], data: T) {
    def js: String = implicitly[CanRender[T]] apply data
  }
  case class FinishedServerScope(pageId: String, comet: ReactionsComet)
  case class ReusingScope(scope: Scope)

  private val _currentScope = new scala.util.DynamicVariable[Scope](DefaultScope)

  def currentScope: Scope = _currentScope.value

  net.liftweb.http.ResourceServer.allow {
    case "reactive-web.js" :: Nil          => true
  }

  @deprecated("Use init(comet=true) instead", "0.1")
  def initComet = init(true)
  /**
   * Call this method in Boot.boot.
   * You should add something like &lt;span class="lift:reactive"/&gt;
   * in your template (or in whichever pages use reactive-web),
   * to include the required javascript, and possibly the comet actor, in your page.
   * @param comet a function Req=>Boolean that specifies whether to enable comet functionality for a request
   */
  def init(comet: net.liftweb.http.Req => Boolean) {
    LiftRules.cometCreation.append {
      case CometCreationInfo(
        t @ "net.liftweb.reactive.ReactionsComet",
        name,
        defaultXml,
        attributes,
        session
        ) =>
        //TODO better (not thread-local/thread-global) connection to actual Page
        val comet = Page.currentPage.comet
        comet.initCometActor(
          session,
          Full(t),
          name,
          defaultXml,
          attributes
        )
        comet
    }
    LiftRules.snippets.append {
      case "reactive" :: Nil =>
        S.request.dmap[NodeSeq => NodeSeq](identity){ req =>
          if (comet(req))
            _ => CurrentPage.is.renderComet
          else
            _ => CurrentPage.is.render
        }
    }
  }

  /**
   * Call this method in Boot.boot.
   * If you want server-initiated reactions, specify true for the comet parameter,
   * to add a comet actor to each page.
   * In either case, you should add something like &lt;span class="lift:reactive"/&gt;
   * in your template (or in whichever pages use reactive-web),
   * to include the required javascript, and possibly the comet actor, in your page.
   */
  def init(comet: Boolean): Unit = init(_ => comet)
  /**
   * Call this method in Boot.boot to disable comet completely.
   * You should add something like &lt;span class="lift:reactive"/&gt;
   * in your template (or in whichever pages use reactive-web),
   * to include the required javascript in your page.
   */
  def init(): Unit = init(false)

  @deprecated("no longer supported; throws runtime exception", "0.2")
  def findPage(id: String): Option[Page] = throw new RuntimeException("no longer available")

  @deprecated("no longer supported; throws runtime exception", "0.2")
  def isPageAlive(id: String): Boolean = throw new RuntimeException("no longer available")

  @deprecated("no longer supported; throws runtime exception", "0.2")
  def removePage(page: Page) = throw new RuntimeException("no longer available")

  @deprecated("no longer supported; throws runtime exception", "0.2")
  def register(page: Page) = throw new RuntimeException("no longer available")

  /**
   * Queues javascript to be rendered in the current Scope.
   */
  def queue[T: CanRender](renderable: T): Unit = {
    lazy val page = currentScope match {
      case CometScope(p) => Some(p.id)
      case _             => Page.currentPageOption map (_.id)
    }
    trace(QueueingJS(page, renderable))
    _currentScope.value queue renderable
  }

  /**
   * Executes code within a "local" scope. All javascript
   * queued within the scope will simply be accumulated and returned.
   * @param p the code block to execute
   * @return the accumulated javascript
   */
  def inLocalScope(p: => Unit): JsCmd = {
    inScope(new LocalScope)(p).js
  }

  @deprecated("Use inLocalScope", "0.1")
  def inClientScope(p: => Unit): JsCmd = inLocalScope(p)

  /**
   * Executes code in the specified Scope, and
   * returns the scope.
   */
  def inScope[T <: Scope](scope: T)(p: => Unit): T = {
    _currentScope.withValue(scope){ p }
    scope
  }

  /**
   * Executes code within a server scope. All javascript queued
   * withing the scope will be sent to the page's comet
   * and it will be flushed to the browser.
   * @tparam T the return type of the code block
   * @param page the Page to queued the javascript in
   * @param p the code block to execute
   * @return the result of the code block
   */
  def inServerScope[T](page: Page)(p: => T): T = {
    val ret = _currentScope.withValue(CometScope(page)) {
      p
    }
    trace(FinishedServerScope(page.id, page.comet))
    page.comet.flush
    ret
  }
  /**
   * If there is already a current scope for the current thread,
   * execute code within that scope. Otherwise, execute it in
   * a new server context associated with the given Page.
   * @tparam T the return type of the code block
   * @param page the Page to associate queued javascript with if there is no current scope
   * @param p the code block to execute
   * @return the result of the code block
   */
  def inAnyScope[T](page: Page)(block: => T): T = {
    _currentScope.value match {
      case s @ CometScope(`page`) =>
        trace(ReusingScope(s))
        block
      case s if Page.currentPageOption == Some(page) =>
        trace(ReusingScope(s))
        block
      case _ =>
        inServerScope(page)(block)
    }
  }
}

/**
 * The comet actor that powers server-initiated updates to the browser.
 * Not used directly by application code.
 */
class ReactionsComet extends CometActor {

  override def initCometActor(theSession: LiftSession, typ: Box[String], name: Box[String], defaultXml: NodeSeq, attributes: Map[String, String]) =
    super.initCometActor(theSession, typ, name, defaultXml, attributes)

  private[reactive] var queued: JsCmd = JsCmds.Noop

  override def lifespan = Full(new Helpers.TimeSpan(60000))

  override def toString = "net.liftweb.reactive.ReactionsComet "+name

  def render = <span/>

  private case class Queue(js: JsCmd)
  private case object Flush
  private case object Take
  override def lowPriority = {
    case Queue(js) =>
      queued &= js
    case Flush =>
      val q = queued
      partialUpdate(q)
      queued = JsCmds.Noop
    case Take =>
      val ret = queued
      queued = JsCmds.Noop
      reply(ret)
  }
  protected[reactive] def queue[T](renderable: T)(implicit canRender: CanRender[T]) = this ! Queue(Run(canRender(renderable)))

  private[reactive] def flush = this ! Flush
  private[reactive] def take: JsCmd = this !! Take match {
    case Full(js: JsCmd) => js
    case _               => JsCmds.Noop
  }
}
