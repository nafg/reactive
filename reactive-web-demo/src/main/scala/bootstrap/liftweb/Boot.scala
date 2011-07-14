package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
  import Helpers._
import net.liftweb.http._
import net.liftweb.sitemap._
  import Loc._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    println("In boot")
    getClass.getClassLoader match {
      case rcl: java.net.URLClassLoader =>
        println("Classpath:" + rcl.getURLs.mkString("\n  ", "\n  ",""))
    }

    def shouldRedirect(r: Req) = !r.request.serverName.endsWith(".tk") &&
      r.request.serverName != "localhost"
    LiftRules.statelessDispatchTable.append {
      case r if shouldRedirect(r) => () => Full(
        PermRedirectResponse("http://reactive-web.tk"+r.uri, r, r.cookies: _*) 
      )
    }

    // where to search snippet
    LiftRules.addToPackages("reactive.web.demo")

    reactive.web.Reactions.init(comet = true)

    // Build SiteMap
    def sitemap = () => SiteMap(
      Menu("About")  /"index",
      Menu("Core")  /"0"  >>PlaceHolder  submenus(
        Menu("EventStream")  /"core"/"EventStream",
        Menu("Signal")  /"core"/"Signal",
        Menu("SeqSignal")  /"core"/"SeqSignal",
        Menu("Forwardable")  /"core"/"Forwardable",
        Menu("Logger")  /"core"/"Logger"
      ),
      Menu("Web")  /"1"  >>PlaceHolder  submenus(
        Menu("Getting Started")  /"web"/"GettingStarted",
        Menu("Low Level API")  /"web"/"LowLevel",
        Menu("Events")  /"web"/"Events",
        Menu("Properties")  /"web"/"Properties",
        Menu("Elements")  /"web"/"Elements",
        Menu("Simple demo")  /"demos"/"SimpleDemo"
      ),
      Menu("HTML")  /"2"  >>PlaceHolder  submenus(
        Menu("Span")  /"html"/"Span",
        Menu("Div")  /"html"/"Div",
        Menu("Button")  /"html"/"Button",
        Menu("TextInput")  /"html"/"TextInput",
        Menu("CheckboxInput")  /"html"/"CheckboxInput",
        Menu("Select")  /"html"/"Select"
      ),
      Menu("Scaladocs")  /"3"  >>PlaceHolder  submenus(
        Menu("reactive-core")  /"reactive-core-api"/ **,
        Menu("reactive-web")  /"reactive-web-api"/ **
      ),
      reactive.web.demo.snippet.DemoPane.menu
    )
    LiftRules.setSiteMapFunc(sitemap)
    LiftRules.liftRequest.append {
      case Req("reactive-core-api"::_, _, _) => false
      case Req("reactive-web-api"::_, _, _) => false
    }
    LiftRules.useXhtmlMimeType = false
  }
}

