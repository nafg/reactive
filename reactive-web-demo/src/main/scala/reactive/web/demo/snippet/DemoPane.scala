package reactive.web.demo.snippet


import scala.xml.NodeSeq

import net.liftweb.http.{S, Templates}

import net.liftweb.util._
  import Helpers._

import net.liftweb.common._

import net.liftweb.sitemap._
  import Loc._

import net.liftweb.doc.snippet.CodeInjection

object DemoPane {
  val menu = Menu.param[String]("ShowDemo","ShowDemo",Full(_),s=>s)  /"showdemo"  >>Hidden
  lazy val loc = menu.toLoc
  
  def render(html: NodeSeq) = {
    (
      for {
        snippetName <- S.attr("snippet") or loc.currentValue
        codePath = ( "/scala-sources/reactive/web/demo/snippet/" + snippetName + ".scala" )
        htmlPath = ( "/templates-hidden/" + snippetName.toLowerCase )
        layout <- Templates(List("templates-hidden", "demopanelayout"))
        template = Templates( htmlPath.split("/").toList ) openOr html
        bind = ".demo [class]" #> ("lift:" + snippetName) &
          ".demo *" #> template &
          ".snippet *" #> CodeInjection.render( codePath ) &
          ".template *" #> CodeInjection.render( template )
      } yield bind(layout)
    ) openOr NodeSeq.Empty
  }
}
