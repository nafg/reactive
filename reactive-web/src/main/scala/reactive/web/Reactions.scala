package reactive
package web


import scala.collection.mutable.{WeakHashMap, HashMap}
import scala.ref.WeakReference
import scala.xml.NodeSeq

import net.liftweb.actor.LiftActor
import net.liftweb.http.{js, CometActor, CometCreationInfo, LiftSession, LiftRules}
  import js.{JsCmd, JsCmds}
    import JsCmds._
import net.liftweb.common.{Box, Full}
import net.liftweb.util.{Helpers, ThreadGlobal}

object Reactions {
  private val pending = new HashMap[String, (JsCmd, Long)]
  private val pages = new WeakHashMap[Page, WeakReference[ReactionsComet]]
  
  private val currentScope = new ThreadGlobal[Either[JsCmd, Page]]
  
  /**
   * Call this method in Boot.boot if you want server-initiated reactions
   */
  def initComet {
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
          name openOr error("Name required for ReactionsComet"),
          defaultXml,
          attributes
        )
        assert(ca.name == Full(CurrentPage.is.id))
        register(CurrentPage.is, ca)
        ca
    }
    LiftRules.snippets.append {
      case "reactive"::Nil => _ => CurrentPage.is.render
    }
  }

  def findPage(id: String): Option[Page] = pages.collect{case (p,_) if p.id==id => p}.headOption
  def isPageAlive(id: String): Boolean = pages.collect{case (p,c) if p.id==id => c.get.isDefined} match {
    case xs: Seq[Boolean] => xs.nonEmpty && xs.exists(identity)
  }

  def register(page: Page, comet: ReactionsComet) = synchronized {
    val pend = pending.remove(page.id) map { case (js,_) => js } getOrElse JsCmds.Noop
    
    pages.get(page).flatMap(_.get) foreach { oldComet =>
      oldComet.flush
       comet queue oldComet.take
    }
    comet queue pend
    comet.flush
    pages(page) = new WeakReference(comet)
  }
  
  def queue(cmd: JsCmd) {
    currentScope.box match {
      case Full(Left(js)) =>
        currentScope.set(Left(js & cmd))
      case Full(Right(page)) =>
        val js =
          pending.remove(page.id).map{case (js,_)=>js}.getOrElse(JsCmds.Noop) &
          cmd
        
        pages.get(page).flatMap(_.get) match {
          case None =>
            println(page.id + ": No page, so pending " + js)
            pending(page.id) = (js, System.currentTimeMillis)
          case Some(comet) =>
            println(page.id + ": Queueing " + js)
            comet queue js
        }
      case _ =>
        error("No Reactions scope")
    }
  }
  def removePage(page: Page) = pages.remove(page)
  
  def inClientScope(p: => Unit): JsCmd = {
    currentScope.doWith(Left(JsCmds.Noop)) {
      p
      currentScope.value.left.get
    }
  }
  def inServerScope[T](page: Page)(p: => T): T = {
    //TODO should we do anything different if page doesn't exist in pages?
    //is it possible the page will still be registered?
    val ret = currentScope.doWith(Right(page)) {
      p
    }
    println(page.id + ": Finished server scope " + page + ": " + pages.get(page))
    pages.get(page).flatMap(_.get) foreach {_.flush}
    ret
  }
  def inAnyScope[T](page: Page)(p: =>T): T = {
    currentScope.box match {
      case Full(scope) =>  // if there is an existing scope do it there
        println(page.id + ": Already in scope: " + scope)
        p
      case _ =>        // otherwise do it in server scope
        inServerScope(page)(p)
    }
  }
}

  
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
  
  def render = <span></span>
  
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
      println(page.id + ": partialUpdating: " + q.toJsCmd)
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
