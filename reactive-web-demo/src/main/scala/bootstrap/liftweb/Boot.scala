package bootstrap.liftweb

import scala.xml.{Elem, NodeSeq}

import net.liftweb.common.Full
import net.liftweb.doc.snippet.CodeInjection
import net.liftweb.http._
import net.liftweb.markdown.ThreadLocalTransformer
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Helpers.StringToCssBindPromoter
import net.liftweb.util.Html5
import reactive.Observing
import reactive.logging._
import reactive.web.lift._
import reactive.web.widgets.Messages
import reactive.web.widgets.lift.MessagesSnippet


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot(): Unit = {
    println("In boot")
    getClass.getClassLoader match {
      case rcl: java.net.URLClassLoader =>
        println("Classpath:" + rcl.getURLs.mkString("\n  ", "\n  ", ""))
    }

    LiftRules.securityRules = () => SecurityRules(content = None)

    // where to search snippets
    LiftRules.addToPackages("reactive.web.demo")

    AppendToRender.init()
    SimpleAjaxTransportType.init()
    SseTransportType.init()
    MessagesSnippet.init(Messages.template("alert"))

    val mdParser = new ThreadLocalTransformer

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
              case other               =>
                elem
            }
        })
      <div data-lift="surround?with=navigable;at=content">
        {renderPres(html)}
      </div>
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
      if (item.children.isEmpty) m >> markdownTemplate
      else m >> emptyPage >> PlaceHolder submenus (item.children.map(i => itemToMenu(i, p)): _*)
    }

    val modules = Seq(
      "reactive-core",
      "reactive-routing",
      "reactive-transport",
      "reactive-jsdsl",
      "reactive-web-base",
      "reactive-web-html",
      "reactive-web-widgets",
      "reactive-web",
      "reactive-web-lift"
    )
    val projSubs =
      (Menu("Combined") / "api" / "unidoc" / **) +:
        modules.map(m => Menu(m) / "api" / m / **)
    val menus = docTree.map(itemToMenu(_)) :+ (
      Menu("Scaladocs") / "3" >> PlaceHolder submenus (projSubs: _*)
      ) :+
      reactive.web.demo.snippet.DemoPane.menu
    def sitemap = () => SiteMap(menus: _*)
    LiftRules.setSiteMapFunc(sitemap)
    LiftRules.liftRequest.append {
      case Req("api" :: _, _, _) => false
    }
    LiftRules.excludePathFromContextPathRewriting.default.set((_: String) => true)
    LiftRules.useXhtmlMimeType = false

    LiftRules.htmlProperties.default.set((r: Req) =>
      Html5Properties(r.userAgent)
    )

    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    Logging.init()

    println("reactive finished booting!")
  }
}

object Logging {
  implicit private object observing extends Observing
  def init(): Unit = {
    Logger.all foreach {
      case (level, LogEvent(subj, pred)) =>
        System.err.println(s"$level ($subj): $pred")
    }
  }
}
