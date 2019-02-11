package reactive
package web
package demo
package snippet


import scala.xml.NodeSeq

import net.liftweb.common._
import net.liftweb.doc.snippet.CodeInjection
import net.liftweb.http.{S, Templates}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Helpers._
import net.liftweb.util._

object DemoPane {
  val menu = Menu.param[String]("ShowDemo","ShowDemo",Full(_),s=>s)  /"showdemo"  >>Hidden
  lazy val loc = menu.toLoc

  def render(xhtml: NodeSeq) = (
    for {
      snippetName <- S.attr("snippet") or loc.currentValue
      layout <- Templates(List("templates-hidden", "demopanelayout"))
    } yield {
      val htmlPath = "/templates-hidden/" + snippetName.toLowerCase
      val templateXml = Templates(htmlPath.split("/").toList) openOr xhtml
      val renderedTemplate: Box[NodeSeq] = CodeInjection.load(htmlPath + ".html") or {
        Full(xhtml) filter (_.flatMap(_.child).toString.trim != "") map ( ns =>
          CodeInjection.renderCodeMirror(ns.toString, "Template", "html")
        )
      }
      val codePath = "/scala-sources/reactive/web/demo/snippet/" + snippetName + ".scala"
      val sel = ".demo [data-lift]" #> snippetName &
        ".demo *" #> templateXml &
        ".snippet *" #> CodeInjection.load(codePath) &
        ".template *" #> renderedTemplate.openOr(NodeSeq.Empty) andThen
        "#template-code" #> renderedTemplate.dmap[NodeSeq => NodeSeq](ClearNodes)(_ => PassThru)
      sel(layout)
    }
  ) openOr NodeSeq.Empty
}
