package reactive
package web
package lift

import reactive.logging._

import scala.xml.{ Node, Unparsed }

import net.liftweb.http.{ LiftResponse, LiftRules }
import net.liftweb.http.{ S, XhtmlResponse }
import net.liftweb.util.LoanWrapper

/**
 * A [[Transport]] that inserts data
 * in the page during its initial render
 */
class RenderTransport extends AccumulatingTransport {
  def currentPriority = 100
  /**
   * The page components that may render to this `Transport`.
   * There may be more than one. For instance, two Lift snippets
   * may actually render to the same web page despite defining
   * separate `Page` instances.
   */
  protected val transportTypes = new AtomicRef(List.empty[AppendToRenderTransportType])
  /**
   * Add `page` to this transport
   * Called in the constructor of [[AppendToRenderTransportType]]
   */
  protected[web] def addTransportType(transportType: AppendToRenderTransportType): Unit = transportTypes.transform(transportType :: _)
  /**
   * Remove `page` from this transport
   * Called by [[destroy]] and [[renderAndDestroy]]
   */
  protected[web] def removeTransportType(transportType: AppendToRenderTransportType): Unit = transportTypes.transform(_ filter (transportType != _))

  def destroy() = transportTypes.get foreach { pc =>
    pc unlinkTransport this
    removeTransportType(pc)
  }
  
  /**
   * Remove connection with all pages,
   * and return the queued data
   */
  def renderAndDestroy(): Seq[Node] = synchronized {
    val pcs = transportTypes.get
    pcs foreach { pc =>
      pc unlinkTransport this
      removeTransportType(pc)
    }
    val include = <script type="text/javascript" src="/classpath/reactive-web.js"/>
    val js =
      <script type="text/javascript">
        { Unparsed("// <![CDATA[\n" + data.mkString(";\n") + "// ]]>") }
      </script>
    if (pcs.nonEmpty) include +: pcs.flatMap(_.page.render) :+ js
    else Nil
  }
  
  override def toString = s"RenderTransport{transportTypes = $transportTypes, data = $data}"
  
  private[web] def currentPages = transportTypes.get.map(_.page)
}

trait AppendToRender extends HasLogger {
  sealed trait DroppedDataLogEvent
  case class DroppedDataNotHtml(response: LiftResponse, data: Seq[Renderable]) extends DroppedDataLogEvent
  case class DroppedDataNoBody(response: LiftResponse, data: Seq[Renderable]) extends DroppedDataLogEvent

  private val currentPageRenders = new AtomicRef(Map.empty[String, RenderTransport])

  /**
   * The [[RenderTransport]] used in the current page render,
   * if any
   */
  def currentPageRender = currentPageRenders.get.get(S.renderVersion)

  /**
   * The [[Page]]s that are known to be
   * created during the current request
   */
  def currentPages = currentPageRender.toList.flatMap(_.currentPages)

  private object GetTransport { def unapply(x: Any) = currentPageRender }
  private object & { def unapply[A](x: A) = Some(x, x) }

  /**
   * This is where we modify HTML `LiftResponse`s.
   * We retrieve the current [[RenderTransport]] (see [[currentPageRender]]),
   * and attempt to append it to the response's `<body>` element.
   */
  // TODO customizability
  protected def transformResponse: PartialFunction[LiftResponse, LiftResponse] = {
    case (xr: XhtmlResponse) & GetTransport(transport) =>
      (NodeLoc(xr.out) \\? "body") match {
        case Some(body) =>
          val nodes = transport.renderAndDestroy()
          val rendered = nodes.foldLeft(body)(_ appendChild _)
          xr.copy(out = rendered.top.node)
        case None =>
          transport.destroy()
          logger.warn(DroppedDataNoBody(xr, transport.data))
          xr
      }
    case otherResponse & GetTransport(transport) =>
      logger.trace(DroppedDataNotHtml(otherResponse, transport.data))
      transport.destroy()
      otherResponse
  }

  /**
   * Install this AppendToRender into Lift.
   *  - Calls `ResourceServer.allow` to serve reactive-web.js
   *  - Adds a request `LoanWrapper` where we associate a
   *    [[RenderTransport]] with the current request
   *  - Installs our response transformer (see [[transformResponse]])
   */
  def init(): Unit = {
    net.liftweb.http.ResourceServer allow {
      case "reactive-web.js" :: Nil => true
    }

    S addAround new LoanWrapper {
      def apply[A](wrapee: => A) = {
        if (S.request == S.originalRequest)
          currentPageRenders.transform(_ + (S.renderVersion -> new RenderTransport))
        try
          wrapee
        finally
          currentPageRender foreach { rt =>
            rt.renderAndDestroy()
            currentPageRenders.transform(_ filter (_._2 ne rt))
          }
      }
    }

    LiftRules.responseTransformers append transformResponse
  }
}

object AppendToRender extends AppendToRender

class AppendToRenderTransportType(val page: Page) extends TransportType {
  def appendToRender: AppendToRender = AppendToRender

  appendToRender.currentPageRender foreach { pr =>
    linkTransport(pr)
    pr.addTransportType(this)
  }
}
