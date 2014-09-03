package reactive
package web
package lift

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scala.xml.{ NodeSeq, Null, UnprefixedAttribute }
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.js.{ JsCmd, JsCmds }

import reactive.logging.HasLogger

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object LiftCometTransportType {
  private val _overrideCometHack = new scala.util.DynamicVariable(Option.empty[LiftCometTransportType#PageComet])
  private var initted = false

  private def overrideCometHack[A](comet: LiftCometTransportType#PageComet)(f: => A): A =
    if (!initted)
      sys.error("LiftCometTransport was not initialized! You must call LiftCometTransportType.init() in boot in order to use LiftCometTransport.")
    else
      _overrideCometHack.withValue(Some(comet))(f)

  /**
   * Install the reactive comet capability into Lift
   */
  def init(): Unit = {
    LiftRules.cometCreation.append {
      case CometCreationInfo("reactive.web.ReactionsComet", name, defaultXml, attributes, session) if _overrideCometHack.value.isDefined =>
        val comet = _overrideCometHack.value.get
        comet.initCometActor(session, Full("reactive.web.ReactionsComet"), name, defaultXml, attributes)
        comet
    }
    initted = true
  }
}

class LiftCometTransportType(page: Page) extends TransportType with HasLogger {
  // Promise that comet actor will be initialized.
  val initPromise = promise[Box[String]]()
  val initFuture = initPromise.future

  class PageComet extends CometActor {
    // Make initCometActor accessible
    override protected[web] def initCometActor(s: LiftSession, t: Box[String], n: Box[String], x: NodeSeq, a: Map[String, String]): Unit = {
      super.initCometActor(s, t, n, x, a)
      initPromise success t
    }

    override def lifespan = Full(60 seconds)

    def render = <span/>

    //TODO cluster output, perhaps via timer
    override def lowPriority = {
      case js: JsCmd => partialUpdate(js)
    }
  }

  val comet = new PageComet

  object cometTransport extends Transport {
    def currentPriority: Int = 0

    queued =>> { renderable => comet ! JsCmds.Run(renderable.render) }
  }

  LiftCometTransportType.overrideCometHack(comet) {
    // This is how we install our own comet actors in Lift
    S.withAttrs(new UnprefixedAttribute("type", "reactive.web.ReactionsComet", new UnprefixedAttribute("name", page.id, Null))) {
      net.liftweb.builtin.snippet.Comet.render(NodeSeq.Empty)
    }
  }

  // Ask comet actor for render after his initialization for avoid NPE
  val askRender = future {
    initFuture onSuccess {
      case Full("reactive.web.ReactionsComet") =>
        comet !? (comet.cometRenderTimeout, AskRender) foreach {
          case AnswerRender(_, _, when, _) =>
            page.queue(s"var lift_toWatch = lift_toWatch || {}; lift_toWatch['${comet.uniqueId}'] = $when;")
        }
    }
  }
  // Waiting for comet actor answer for render lift_toWatch
  Await.result(askRender, Duration(5, TimeUnit.SECONDS))

  override def render = super.render ++ S.session.map{ session =>
    // TODO ensure that it's not already rendered
    val cometSrc =
      List(S.contextPath, LiftRules.cometPath, urlEncode(session.uniqueId), LiftRules.cometScriptName()) mkString "/"
    <script src={ S.encodeURL(cometSrc) } type="text/javascript"/>
  }.openOr{ logger.warn("Rendering "+this+" outside of session"); NodeSeq.Empty }

  linkTransport(cometTransport)
}
