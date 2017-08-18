package reactive
package web
package lift

import scala.xml.NodeSeq

import net.liftweb.builtin.snippet.Comet
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.util.Helpers._
import reactive.logging.HasLogger


class LiftCometTransportType(page: Page) extends TransportType with HasLogger {
  class PageComet extends CometActor {
    // Make initCometActor accessible
    override protected[web] def initCometActor(cci: CometCreationInfo): Unit = super.initCometActor(cci)

    override def lifespan = Full(60.seconds)

    def render = <span/>

    //TODO cluster output, perhaps via timer
    override def lowPriority = {
      case js: JsCmd => partialUpdate(js)
    }
  }

  val comet = new PageComet

  object cometTransport extends Transport {
    def currentPriority: Int = 0
    queued foreach { renderable => comet ! JsCmds.Run(renderable.render) }
  }

  override def render = super.render ++ Comet.containerForCometActor(comet)

  S.session.foreach { session =>
    val cci = CometCreationInfo("reactive.web.ReactionsComet", Full(page.id), NodeSeq.Empty, Map.empty, session)
    comet.initCometActor(cci)
    session.buildAndStoreComet(_ => Full(comet))(cci)
  }
  S.addComet(comet)
  linkTransport(cometTransport)
}
