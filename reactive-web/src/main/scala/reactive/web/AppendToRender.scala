package reactive
package web

import scala.xml.{ Node, Unparsed }

import net.liftweb.actor.LiftActor
import net.liftweb.http._
import net.liftweb.util.LoanWrapper

object & { def unapply[A](x: A) = Some(x, x) }

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
      val js = <script type="text/javascript">
        { Unparsed("// <![CDATA[\n" + data.mkString(";\n") + "// ]]>") }
      </script>
      if(ps.nonEmpty) include +: ps.flatMap(_.render) :+ js
      else Nil
    }

    override def toString = s"AppendToRender.Transport{pages = $pages, data = $data}"
  }

  val currentPageRenders = new AtomicRef(List.empty[(String, Transport)])

  def currentPageRender = currentPageRenders.get.find(_._1 == S.renderVersion).map(_._2)

  private object GetTransport { def unapply(x: Any) = currentPageRender }

  // TODO customizability
  def transformResponse: PartialFunction[LiftResponse, LiftResponse] = {
    case (xr: XhtmlResponse) & GetTransport(transport) =>
      val body = NodeLoc(xr.out) \\! "body"
      val nodes = transport.renderAndDestroy()
      println("Append to render: "+nodes)
      val rendered = nodes.foldLeft(body)(_ appendChild _)
      xr.copy(out = rendered.top.node)
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
        println(s"Beginning request ${S.renderVersion}, ${S.request}, original = ${S.originalRequest}")
        if(S.request == S.originalRequest)
          currentPageRenders.transform((S.renderVersion, new Transport) :: _)
        val ret = try f finally {
          val cur = currentPageRender
          cur foreach (_.renderAndDestroy())
          currentPageRenders.transform(_ filter (cur ne _))
        }
        println(s"Ending request ${S.renderVersion}")
        println(ret)
        ret
      }
    }

    LiftRules.responseTransformers append transformResponse
  }
}

trait AppendToRenderPage extends Page {
  def appendToRender: AppendToRender = AppendToRender

  appendToRender.currentPageRender foreach linkTransport
}
