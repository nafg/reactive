package reactive
package web
package snippet


import scala.xml.NodeSeq

import net.liftweb.http.{S, TemplateFinder}

import net.liftweb.util._
  import Helpers._

import net.liftweb.common._


class DemoPane {
  def render(template: NodeSeq) = (
    for {
      snippetName <- S.attr("snippet")
      layout <- TemplateFinder.findAnyTemplate(List("templates-hidden", "demopanelayout"))
      scalaSource = scala.io.Source.fromInputStream(
        getClass.getResourceAsStream("/scala-sources/reactive/web/snippet/" + snippetName + ".scala")
      )
      bind = ".demo [class]" #> ("lift:" + snippetName) &
        ".demo *" #> template &
        ".template *" #> <pre class="brush: xml">{template.toString}</pre> &
        ".snippet *" #> <pre class="brush: scala">{scalaSource.getLines().mkString("\n")}</pre>
    } yield {
      val ret = bind(layout)
      println("Output: " + ret)
      ret
    }
  ) openOr NodeSeq.Empty
}
