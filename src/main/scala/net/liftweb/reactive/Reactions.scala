package net.liftweb.reactive

import scala.collection.mutable.{WeakHashMap, HashMap}
import scala.xml.NodeSeq

import net.liftweb.actor.LiftActor
import net.liftweb.http.{js, CometActor, CometCreationInfo, LiftSession, LiftRules}
  import js.{JsCmd, JsCmds}
    import JsCmds._
import net.liftweb.common.{Box, Full}
import net.liftweb.util.{Helpers, ThreadGlobal}

object Reactions {
  private val pending = new HashMap[String, (JsCmd, Long)] 
  private val pages = new WeakHashMap[String, ReactionsComet]
  
  private val currentScope = new ThreadGlobal[Either[JsCmd, Page]]
  
  /**
   * Call this method in Boot.boot
   */
  def init {
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
        println("In cometCreation")
        assert(ca.name == Full(CurrentPage.is.id))
        register(CurrentPage.is.id, ca)
        ca
    }
  }
  
  def register(page: String, comet: ReactionsComet) = synchronized {
    val pend = pending.remove(page) map { case (js,_) => js } getOrElse JsCmds.Noop
    
    pages.get(page) foreach { oldComet =>
      oldComet.flush
      comet queue oldComet.take
    }
    comet queue pend
    comet.flush
    pages(page) = comet
  }
  
  def queue(cmd: JsCmd) {
    currentScope.box match {
      case Full(Left(js)) =>
        currentScope.set(Left(js & cmd))
      case Full(Right(p)) =>
        val page = p.id
        val js =
          pending.remove(page).map{case (js,_)=>js}.getOrElse(JsCmds.Noop) &
          cmd
        
        pages.get(page) match {
          case None =>
            pending(page) = (js, System.currentTimeMillis)
          case Some(comet) =>
            comet queue js
        }
      case _ =>
        error("No Reactions scope")
    }
  }
  def inClientScope(p: => Unit): JsCmd = {
    currentScope.doWith(Left(JsCmds.Noop)) {
      p
      currentScope.value.left.get
    }
  }
  def inServerScope(page: Page)(p: => Unit): Unit = {
    currentScope.doWith(Right(page)) {
      p
    }
    pages.get(page.id) foreach {_.flush}
  }
  def inAnyScope(page: Page)(p: =>Unit): Unit = {
    currentScope.box match {
      case Full(_) =>  // if there is an existing scope do it there
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

  private case class Queue(js: JsCmd)
  private case object Flush
  private case object Take
  override def lowPriority = {
    case Queue(js) =>
      queued &= js
    case Flush =>
      println("Flushing: " + queued.toJsCmd)
      println("Page: " + page.id)
      println("CA: " + this)
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
