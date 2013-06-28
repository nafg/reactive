package reactive
package web

import scala.xml.{ Node, Unparsed }

import net.liftweb.http.{ LiftResponse, LiftRules }
import net.liftweb.http.{ S, XhtmlResponse }
import net.liftweb.util.LoanWrapper

object AppendToRender extends AppendToRender

trait AppendToRender extends Logger {
  case class DroppedData(reponse: LiftResponse, data: Seq[String])

  class Transport extends AccumulatingTransport {
    def currentPriority = 100

    /**
     * Remove connection with all pages,
     * and return the queued data
     */
    def renderAndDestroy(): Seq[Node] = synchronized {
      val ps = pages.get
      ps foreach (_ unlinkTransport this)

      val include = <script type="text/javascript" src="/classpath/reactive-web.js"/>
      val js =
        <script type="text/javascript">
          { Unparsed("// <![CDATA[\n" + data.mkString(";\n") + "// ]]>") }
        </script>
      if(ps.nonEmpty) include +: ps.flatMap(_.render) :+ js
      else Nil
    }

    override def toString = s"AppendToRender.Transport{pages = $pages, data = $data}"

    private[AppendToRender] def currentPages = pages.get
  }

  private val currentPageRenders = new AtomicRef(List.empty[(String, Transport)])

  /**
   * The [[Transport]] used in the current page render,
   * if any
   */
  def currentPageRender = currentPageRenders.get.find(_._1 == S.renderVersion).map(_._2)

  /**
   * The [[AppendToRenderPage]]s that are known to be
   * created during the current request
   */
  def currentPages = currentPageRender.toList.flatMap(_.currentPages)

  private object GetTransport { def unapply(x: Any) = currentPageRender }
  private object & { def unapply[A](x: A) = Some(x, x) }

  // TODO customizability
  def transformResponse: PartialFunction[LiftResponse, LiftResponse] = {
    case (xr: XhtmlResponse) & GetTransport(transport) =>
      val nodes = transport.renderAndDestroy()
      (NodeLoc(xr.out) \\? "body") match {
        case Some(body) =>
          val rendered = nodes.foldLeft(body)(_ appendChild _)
          xr.copy(out = rendered.top.node)
        case None =>
          xr
      }
    case lr & GetTransport(transport) =>
      error(DroppedData(lr, transport.data))
      transport.renderAndDestroy()
      lr
  }

  def init(): Unit = {
    net.liftweb.http.ResourceServer allow {
      case "reactive-web.js" :: Nil => true
    }

    S addAround new LoanWrapper {
      def apply[A](f: => A) = {
        if(S.request == S.originalRequest)
          currentPageRenders.transform((S.renderVersion, new Transport) :: _)
        try f finally {
          val cur = currentPageRender
          cur foreach (_.renderAndDestroy())
          currentPageRenders.transform(_ filter (cur ne _))
        }
      }
    }

    LiftRules.responseTransformers append transformResponse
  }
}

trait AppendToRenderPage extends Page {
  def appendToRender: AppendToRender = AppendToRender

  appendToRender.currentPageRender foreach linkTransport
}
