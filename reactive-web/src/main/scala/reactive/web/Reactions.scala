package reactive
package web


import scala.collection.mutable.{WeakHashMap, HashMap}
import scala.ref.WeakReference
import scala.xml.NodeSeq

import net.liftweb.actor.LiftActor
import net.liftweb.http.{js, S, CometActor, CometCreationInfo, LiftSession, LiftRules}
  import js.{JsCmd, JsCmds}
    import JsCmds._
import net.liftweb.common.{Box, Full}
import net.liftweb.util.{Helpers, ThreadGlobal}


/**
 * This singleton keeps track of pages and queued javascript
 */
object Reactions extends Logger {
  case class PendingJS(pageId: String, js: JsCmd) extends LogEventPredicate
  case class QueueingJS(pageId: String, js: JsCmd) extends LogEventPredicate
  case class FinishedServerScope(pageId: String, comet: Option[ReactionsComet]) extends LogEventPredicate
  case class ReusingScope(scope: Scope) extends LogEventPredicate

  private val pending = new HashMap[String, (JsCmd, Long)]
  //TODO is WeakHashMap the correct structure to use?
  private val pages = new WeakHashMap[Page, WeakReference[ReactionsComet]]

  sealed trait Scope {
    def queue(cmd: JsCmd)
  }
  case class CometScope(page: Page) extends Scope {
    def queue(cmd: JsCmd) {
      val js =
        pending.remove(page.id).map { case (js, _) => js }.getOrElse(JsCmds.Noop) &
          cmd
      pages.get(page).flatMap(_.get) match {
        case None =>
          trace(PendingJS(page.id, js))
          pending(page.id) = (js, System.currentTimeMillis)
        case Some(comet) =>
          trace(QueueingJS(page.id, js))
          comet queue js
      }
    }
  }
  class AjaxScope extends Scope {
    var js: JsCmd = JsCmds.Noop
    def queue(cmd: JsCmd) = js &= cmd
  }
  case object DefaultScope extends Scope {
    def queue(cmd: JsCmd) = S.appendJs(cmd)
  }
  private val currentScope = new scala.util.DynamicVariable[Scope](DefaultScope)
  
  net.liftweb.http.ResourceServer.allow {
    case "reactive-web.js"::Nil => true
  }

  @deprecated("Use init(comet=true) instead")
  def initComet = init(true)
  /**
   * Call this method in Boot.boot.
   * If you want server-initiated reactions, specify true for the comet parameter,
   * to add a comet creation handler.
   * In either case, you should add something like &lt;span class="lift:reactive"/&gt;
   * in your template (or in whichever pages use reactive-web),
   * to include the required javascript, and possibly the comet actor, in your page.
   */
  def init(comet: Boolean = false) {
    if (comet) {
      LiftRules.cometCreation.append {
        case CometCreationInfo(
          "net.liftweb.reactive.ReactionsComet",
          name,
          defaultXml,
          attributes,
          session
          ) =>
          val ca = new ReactionsComet(
            session,
            name openOr (throw new IllegalArgumentException("Name required for ReactionsComet")),
            defaultXml,
            attributes
          )
          assert(ca.name == Full(CurrentPage.is.id))
          register(CurrentPage.is, ca)
          ca
      }
      LiftRules.snippets.append {
        case "reactive" :: Nil => _ => CurrentPage.is.renderComet
      }
    } else {
      LiftRules.snippets.append {
        case "reactive" :: Nil => _ => CurrentPage.is.render
      }
    }
  }

  /**
   * Find a page by id
   * @param id the id of the Page to look for
   * @return the Page, if any is found
   */
  def findPage(id: String): Option[Page] = pages.collect{case (p,_) if p.id==id => p}.headOption
  /**
   * Queries whether a page with the given id exists and its comet has not been garbage collected 
   */
  def isPageAlive(id: String): Boolean = pages.nonEmpty && pages.exists {
    case (p,c) if p.id==id && c.get.isDefined => true
    case _ => false
  }

  /**
   * Registers a Page and comet actor with the system.
   * If a comet already exists for the page, it is flushed and any remaining javascript
   * is queued in the new comet actor. Any javascript that was pending for the page
   * (e.g., was queued before a comet actor was registered) is queued in the new comet
   * actor and it is flushed.
   * A WeakReference to the comet actor is stored in a WeakHashMap with the Page as the key.   
   */
  def register(page: Page, comet: ReactionsComet) = synchronized {
    val pend = pending.remove(page.id) map { case (js,_) => js } getOrElse JsCmds.Noop

    pages.get(page).flatMap(_.get) foreach { oldComet =>
      oldComet.flush
      //TODO what's that point of take? Won't it always be Noop at this point?
      comet queue oldComet.take
    }
    comet queue pend
    comet.flush
    pages(page) = new WeakReference(comet)
  }
 
  /**
   * Queues javascript to be rendered.
   * The current thread must be in a valid context scope.
   * If the current thread is in a client context, the
   * javascript is added to the queue for the current scope.
   * If the thread is in a server-context scope, then the
   * javascript, plus any pending for the Page of the context,
   * is queued to the comet registered for the Page, if there
   * is one, or if there is not then it is stored pending.
   */
  def queue(cmd: JsCmd) =
    currentScope.value queue cmd
  
  /**
   * Unregister a Page. Removes it from the WeakHashMap.
   */
  def removePage(page: Page) = pages.remove(page)
  
  /**
   * Executes code within a client scope. All javascript
   * queued within the scope will be accumulated and returned.
   * @param p the code block to execute
   * @return the accumulated javascript
   */
  def inClientScope(p: => Unit): JsCmd = {
    val scope = new AjaxScope
    currentScope.withValue(scope) {
      p
      scope.js
    }
  }
  /**
   * Executes code within a server scope. All javascript queued
   * withing the scope will be sent to the comet actor registered
   * for the page and it will be flushed, if there is one,
   * or else it will be stored pending.
   * @tparam the return type of the code block
   * @param page the Page to associate queued javascript with
   * @param p the code block to execute
   * @return the result of the code block
   */
  def inServerScope[T](page: Page)(p: => T): T = {
    //TODO should we do anything different if page doesn't exist in pages?
    //is it possible the page will still be registered?
    val ret = currentScope.withValue(CometScope(page)) {
      p
    }
    val comet = pages.get(page).flatMap(_.get)
    trace(FinishedServerScope(page.id, comet))
    comet foreach (_.flush)
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
  def inAnyScope[T](page: Page)(block: =>T): T = {
    currentScope.value match {
      case s@CometScope(p) if p == Page =>
        trace(ReusingScope(s))
        block
      case s if Page.currentPageOption==Some(page)=>
        trace(ReusingScope(s))
        block
      case _ =>
        inServerScope(page)(block)
    }
  }
}


/**
 * The comet actor that powers server-initiated updates to the browser.
 * Not used by application code
 */
class ReactionsComet(
  theSession: LiftSession,
  name: String,
  defaultXml: NodeSeq,
  attributes: Map[String, String]
) extends CometActor {
  
  super.initCometActor(theSession, Full("net.liftweb.reactive.ReactionsComet"), Full(name), defaultXml, attributes)
  
  private[reactive] val page: Page = CurrentPage.is
  
  private[reactive] var queued: JsCmd = JsCmds.Noop 
  
  override def lifespan = Full(new Helpers.TimeSpan(60000))
  
  override def toString = "net.liftweb.reactive.ReactionsComet " + name
  
  def render = <span/>
  
  override protected def localShutdown {
    Reactions.removePage(page)
  }

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
  private[reactive] def queue(js: JsCmd) = this ! Queue(js)
  private[reactive] def flush = this ! Flush
  private[reactive] def take: JsCmd = this !! Take match {
    case Full(js: JsCmd) => js
    case _ => JsCmds.Noop
  }  
}
