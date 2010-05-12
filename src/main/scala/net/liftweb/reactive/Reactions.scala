package net.liftweb.reactive

import scala.collection.mutable.{WeakHashMap, HashMap}
import scala.xml.NodeSeq

import net.liftweb.actor.LiftActor
import net.liftweb.http.{js, CometActor, CometCreationInfo, LiftSession, LiftRules}
  import js.{JsCmd, JsCmds}
    import JsCmds._
import net.liftweb.common.{Box, Full}
import net.liftweb.util.Helpers

object Reactions extends LiftActor {
  private val pending = HashMap[String, (JsCmd, Long)]() 
  private val pages = WeakHashMap[String, comet.ReactionsComet]()
  
  /**
   * Call this method in Boot.boot
   */
  def init {
    LiftRules.cometCreation.append {
      case CometCreationInfo(
        "net.liftweb.reactive.comet.ReactionsComet",
        name,
        defaultXml,
        attributes,
        session
      ) =>
        val ca = new comet.ReactionsComet(
          session,
          Full("net.liftweb.reactive.comet.ReactionsComet"),
          name,
          defaultXml,
          attributes
        )
        println("In cometCreation")
        assert(ca.name == Full(CurrentPage.is.id))
        this ! Register(CurrentPage.is.id, ca)
        ca
    }
  }
  
  
  def messageHandler = {
    case Register(page: String, comet) =>
      val pend = pending.remove(page) map { case (js,_) => js } getOrElse JsCmds.Noop
      
      pages.get(page) foreach { oldComet =>
        oldComet.flush
        comet queue oldComet.take
      }
      comet queue pend
      comet.flush
      pages(page) = comet
    case Queue(page, cmd) =>
      println("Enqueueing " + cmd)
      println("Comets: " + pages.size)
      val js =
        pending.remove(page).map{case (js,_)=>js}.getOrElse(JsCmds.Noop) &
        cmd
      
      pages.get(page) match {
        case None =>
          pending(page) = (js, System.currentTimeMillis)
        case Some(comet) =>
          comet queue js
      }
    case Flush(page) =>
      pages.get(page) foreach {_.flush}
  }
  private case class Register(page: String, comet0: comet.ReactionsComet)
  private case class Queue(page: String, cmd: JsCmd)
  private case class Flush(page: String)
  
  def queueCmd(cmd: JsCmd)(implicit page: Page) =
    this ! Queue(page.id, cmd)
  def flushQueue(implicit page: Page) =
    this ! Flush(page.id)
  
}

  
package comet {
class ReactionsComet(
  theSession: LiftSession,
  theType: Box[String],
  name: Box[String],
  defaultXml: NodeSeq,
  attributes: Map[String, String]
) extends CometActor {
  
  super.initCometActor(theSession, theType, name, defaultXml, attributes)
  
  private[reactive] val page: Page = CurrentPage.is
  
  private[reactive] var queued: JsCmd = JsCmds.Noop 
  
//  override def lifespan = Full(new Helpers.TimeSpan(60000))
  
  override def toString = "net.liftweb.reactive.comet.ReactionsComet " + name
  
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
}
