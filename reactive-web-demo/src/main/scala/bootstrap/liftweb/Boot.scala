package bootstrap.liftweb

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._

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
    
    // where to search snippet
    LiftRules.addToPackages("reactive.web")

    reactive.web.Reactions.initComet
    
    // Build SiteMap
    def sitemap = () => SiteMap(
      Menu("Core")  /"0"  >>PlaceHolder  submenus(
        Menu("EventStream")  /"demos"/"core"/"EventStream",
        Menu("Signal")  /"demos"/"core"/"Signal"
      ),
      Menu("Web")  /"index",
      Menu("Scaladocs")  /"1"  >>PlaceHolder  submenus(
        Menu("reactive-core")  /"reactive-core-api"/ **,
        Menu("reactive-web")  /"reactive-web-api"/ **
      )
    )
    LiftRules.setSiteMapFunc(sitemap)
    LiftRules.liftRequest.append {
      case Req("reactive-core-api"::_, _, _) => false
      case Req("reactive-web-api"::_, _, _) => false
    }
    LiftRules.useXhtmlMimeType = false
  }
}

