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
object Reactions extends Reactions

trait Reactions extends Logger {
  net.liftweb.http.ResourceServer.allow {
    case "reactive-web.js" :: Nil          => true
  }

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
            _ => Page.currentPage.renderComet
          else
            _ => Page.currentPage.render
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
