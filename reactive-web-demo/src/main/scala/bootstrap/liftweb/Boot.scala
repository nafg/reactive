package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import Loc._

import reactive.web.widgets.Messages

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    println("In boot")
    getClass.getClassLoader match {
      case rcl: java.net.URLClassLoader =>
        println("Classpath:"+rcl.getURLs.mkString("\n  ", "\n  ", ""))
    }

    def shouldRedirect(r: Req) = !r.request.serverName.endsWith(".co.cc") &&
      r.request.serverName != "localhost"
    LiftRules.statelessDispatchTable.append {
      case r if shouldRedirect(r) => () => Full(
        PermRedirectResponse("http://reactive-web.co.cc"+r.uri, r, r.cookies: _*)
      )
    }

    // where to search snippets
    LiftRules.addToPackages("reactive.web.demo")

    reactive.web.Reactions.init(comet = true)
    Messages.init(Messages.template("alert"))

    // Build SiteMap
    def sitemap = () => SiteMap(
      Menu("About") / "index",
      Menu("Core") / "0" >> PlaceHolder submenus (
        Menu("EventStream") / "core" / "EventStream",
        Menu("Signal") / "core" / "Signal",
        Menu("SeqSignal") / "core" / "SeqSignal",
        Menu("Forwardable") / "core" / "Forwardable",
        Menu("Logger") / "core" / "Logger",
        Menu("Recipes") / "core" / "Recipes"
      ),
      Menu("Web") / "1" >> PlaceHolder submenus (
        Menu("Getting Started") / "web" / "GettingStarted",
        Menu("Low Level API") / "web" / "LowLevel",
        Menu("Javascript") / "web" / "JsEventStream",
        Menu("Events") / "web" / "Events",
        Menu("Properties") / "web" / "Properties",
        Menu("Elements") / "web" / "Elements",
        Menu("HTML Classes") / "web" / "Html",
        Menu("Testing") / "web" / "TestScope",
        Menu("Configuration") / "web" / "Config",
        Menu("Simple demo") / "demos" / "SimpleDemo"
      ),
      Menu("Widgets") / "2" >> PlaceHolder submenus (
        Menu("Messages") / "widgets" / "Messages"
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
    LiftRules.useXhtmlMimeType = false

    LiftRules.htmlProperties.default.set( (r: Req) =>
      new Html5Properties(r.userAgent)
    )

    LiftRules.early.append( _.setCharacterEncoding("UTF-8") )

  }
}

