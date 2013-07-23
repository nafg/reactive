package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
import Helpers.strToCssBindPromoter
import net.liftweb.http._
import net.liftweb.sitemap._
import Loc._
import reactive.web._
import reactive.web.widgets.{ Messages, MessagesSnippet }
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
        println("Classpath:" + rcl.getURLs.mkString("\n  ", "\n  ", ""))
    }

    def shouldRedirect(r: Req) = !r.request.serverName.endsWith(".tk") &&
      r.request.serverName != "localhost"
    LiftRules.statelessDispatch.append {
      case r if shouldRedirect(r) => () => Full(
        PermRedirectResponse("http://reactive-web.tk" + r.uri, r, r.cookies: _*)
      )
    }

    // where to search snippets
    LiftRules.addToPackages("reactive.web.demo")
    LiftRules.addToPackages("com.damianhelme.tbutils")

    AppendToRender.init()
    SimpleAjaxPage.init()
    SsePage.init()
    MessagesSnippet.init(Messages.template("alert"))

    val mdParser = new net.liftweb.markdown.ActuariusTransformer

    def loadMarkdown(path: String): Elem = {
      val f = s"/site/$path.md"
      val res = LiftRules.loadResourceAsString(f) openOr ""
      val md = "<div>" + mdParser(res) + "</div>"
      val html = Html5.parse(md) openOr scala.xml.NodeSeq.Empty
      val brushRe = """\bbrush:\s*(\w+)""".r
      val renderPres =
        "pre" #> ((_: NodeSeq) match {
          case elem: Elem =>
            elem.attribute("class").map(_.text) match {
              case Some(brushRe(lang)) =>
                CodeInjection.renderCodeMirror(elem.text, "", lang)
              case other =>
                elem
            }
        })
      <div data-lift="surround?with=navigable;at=content">{
        renderPres(html)
      }</div>
    }
    def markdownTemplate = Template { () =>
      val path = S.uri stripPrefix "/" stripSuffix "/" match {
        case "" => "index"
        case x  => x
      }
      loadMarkdown(path)
    }

    def emptyPage = Template { () =>
      <div data-lift="surround?with=navigable;at=content"/>
    }
    def header = MenuCssClass("nav-header")

    // Build SiteMap

    object Item {
      def apply(name: String): Item = new Item(name, name)
    }
    case class Item(name: String, title: String, children: Item*)
    def docTree =
      Seq(
        Item("index", "About"),
        Item("core", "Core",
          Item("EventStream", "EventStream",
            Item("intro", "Introduction"),
            Item("EventSource", "Creating"),
            Item("timer", "Timer"),
            Item("foreach"),
            Item("transform", "Transformations"),
            Item("merge", "Merge"),
            Item("hold"),
            Item("other", "Other operations")
          ),
          Item("Signal"),
          Item("SeqSignal"),
          Item("Forwardable"),
          Item("Logger"),
          Item("Recipes")
        ),
        Item("web", "Web",
          Item("GettingStarted", "Getting started"),
          Item("LowLevel", "Low level API"),
          Item("JsEventStream", "Javascript"),
          Item("Events"),
          Item("Properties"),
          Item("Elements"),
          Item("Html", "HTML classes"),
          Item("TestTransport", "Testing"),
          Item("Config", "Configuration")
        ),
        Item("widgets", "Widgets",
          Item("Messages")
        )
      )

    def itemToMenu(item: Item, path: List[String] = Nil): Menu.Menuable = {
      val p = path :+ item.name
      val m = p.tail.foldLeft(Menu(item.name, item.title) / p.head)(_ / _)
      if(item.children.isEmpty) m >> markdownTemplate
      else m >> emptyPage >> PlaceHolder submenus (item.children.map(i => itemToMenu(i, p)): _*)
    }
    val menus = docTree.map(itemToMenu(_)) :+ (
      Menu("Scaladocs") / "3" >> PlaceHolder submenus (
        Menu("reactive-core") / "reactive-core-api" / **,
        Menu("reactive-web") / "reactive-web-api" / **
      )
    ) :+
        reactive.web.demo.snippet.DemoPane.menu
    def sitemap = () => SiteMap(menus: _*)
    LiftRules.setSiteMapFunc(sitemap)
    LiftRules.liftRequest.append {
      case Req("reactive-core-api" :: _, _, _) => false
      case Req("reactive-web-api" :: _, _, _)  => false
    }
    LiftRules.excludePathFromContextPathRewriting.default.set{ _: String => true }
    LiftRules.useXhtmlMimeType = false

    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent)
    )

    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    println("reactive finished booting!")
  }
}
