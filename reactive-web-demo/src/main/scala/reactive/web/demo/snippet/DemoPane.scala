package reactive.web.demo.snippet


import scala.xml.NodeSeq

import net.liftweb.http.{S, Templates}

import net.liftweb.util._
  import Helpers._

import net.liftweb.common._

import net.liftweb.sitemap._
  import Loc._


object DemoPane {
  val menu = Menu.param[String]("ShowDemo","ShowDemo",Full(_),s=>s)  /"showdemo"  >>Hidden
  lazy val loc = menu.toLoc
  
  def render(xhtml: NodeSeq) = (
    for {
      snippetName <- S.attr("snippet") or loc.currentValue
      layout <- Templates(List("templates-hidden", "demopanelayout"))
      scalaSource = scala.io.Source.fromInputStream(
        getClass.getResourceAsStream("/scala-sources/reactive/web/demo/snippet/" + snippetName + ".scala")
      )
      template = Templates(List("templates-hidden", snippetName.toLowerCase)) openOr xhtml
      bind = ".demo [class]" #> ("lift:" + snippetName) &
        ".demo *" #> template &
        ".template *" #> <pre class="brush: xml">{template.toString}</pre> &
        ".snippet *" #> <pre class="brush: scala">{scalaSource.getLines().mkString("\n")}</pre>
    } yield bind(layout)
  ) openOr NodeSeq.Empty
}
