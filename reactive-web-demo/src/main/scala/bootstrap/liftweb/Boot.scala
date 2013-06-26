package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
import Helpers.strToCssBindPromoter
import net.liftweb.http._
import net.liftweb.sitemap._
import Loc._
import reactive.web._
import reactive.web.widgets.Messages
import scala.xml.Elem
import scala.xml.NodeSeq
import net.liftweb.doc.snippet.CodeInjection

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot() {
    println("In boot")
    getClass.getClassLoader match {
      case rcl: java.net.URLClassLoader =>
        println("Classpath:"+rcl.getURLs.mkString("\n  ", "\n  ", ""))
    }

    def shouldRedirect(r: Req) = !r.request.serverName.endsWith(".tk") &&
      r.request.serverName != "localhost"
    LiftRules.statelessDispatch.append {
      case r if shouldRedirect(r) => () => Full(
        PermRedirectResponse("http://reactive-web.tk"+r.uri, r, r.cookies: _*)
      )
    }

    // where to search snippets
    LiftRules.addToPackages("reactive.web.demo")
    LiftRules.addToPackages("com.damianhelme.tbutils")

    AppendToRender.init()
    SimpleAjaxPage.init()
    SsePage.init()
    Messages.init(Messages.template("alert"))

    val mdParser = new net.liftweb.markdown.ActuariusTransformer

    def loadMarkdown = Template { () =>
      val f = s"/site/${ S.uri stripPrefix "/" stripSuffix "/" }.md"
      val res = LiftRules.loadResourceAsString(f) openOr ""
      val md = "<div>" + mdParser(res) + "</div>"
      val html = Html5.parse(md) openOr scala.xml.NodeSeq.Empty
      val brushRe = """\bbrush:\s*(\w+)""".r
      val renderPres =
        "pre" #> ((_: NodeSeq) match {
          case elem: Elem =>
            elem.attribute("class").map(_.text) match {
              case Some(brushRe(lang)) =>
                CodeInjection.renderCodeMirror(elem.text.stripLineEnd, "", lang)
              case other =>
                elem
            }
        })
      <div data-lift="surround?with=navigable;at=content">{
        renderPres(html)
      }</div>
    }

    // Build SiteMap
    def sitemap = () => SiteMap(
      Menu("About") / "index" >> loadMarkdown,
      Menu("Core") / "0" >> PlaceHolder submenus (
        Menu("EventStream") / "core" / "EventStream" >> loadMarkdown,
        Menu("Signal") / "core" / "Signal" >> loadMarkdown,
        Menu("SeqSignal") / "core" / "SeqSignal" >> loadMarkdown,
        Menu("Forwardable") / "core" / "Forwardable" >> loadMarkdown,
        Menu("Logger") / "core" / "Logger" >> loadMarkdown,
        Menu("Recipes") / "core" / "Recipes" >> loadMarkdown
      ),
      Menu("Web") / "1" >> PlaceHolder submenus (
        Menu("Getting Started") / "web" / "GettingStarted" >> loadMarkdown,
        Menu("Low Level API") / "web" / "LowLevel" >> loadMarkdown,
        Menu("Javascript") / "web" / "JsEventStream" >> loadMarkdown,
        Menu("Events") / "web" / "Events" >> loadMarkdown,
        Menu("Properties") / "web" / "Properties" >> loadMarkdown,
        Menu("Elements") / "web" / "Elements" >> loadMarkdown,
        Menu("HTML Classes") / "web" / "Html" >> loadMarkdown,
        Menu("Testing") / "web" / "TestTransport" >> loadMarkdown,
        Menu("Configuration") / "web" / "Config" >> loadMarkdown,
        Menu("Simple demo") / "demos" / "SimpleDemo"
      ),
      Menu("Widgets") / "2" >> PlaceHolder submenus (
        Menu("Messages") / "widgets" / "Messages" >> loadMarkdown
      ),
      Menu("Scaladocs") / "3" >> PlaceHolder submenus (
        Menu("reactive-core") / "reactive-core-api" / **,
        Menu("reactive-web") / "reactive-web-api" / **
      ),
      reactive.web.demo.snippet.DemoPane.menu
    )
    LiftRules.setSiteMapFunc(sitemap)
    LiftRules.liftRequest.append {
      case Req("reactive-core-api" :: _, _, _) => false
      case Req("reactive-web-api" :: _, _, _)  => false
    }
    LiftRules.excludePathFromContextPathRewriting.default.set{ _: String => true }
    LiftRules.useXhtmlMimeType = false

    LiftRules.htmlProperties.default.set( (r: Req) =>
      new Html5Properties(r.userAgent)
    )

    LiftRules.early.append( _.setCharacterEncoding("UTF-8") )
  }
}
