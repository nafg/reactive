package net.liftweb.reactive

import net.liftweb.util.Helpers.randomString
import net.liftweb.http.{RequestVar, S}

import reactive._

object CurrentPage extends RequestVar(new Page)

trait ReactiveSnippet {
  private var lastPage: Option[Page] = None
  implicit def currentPage: Page =
    if(S.request.isEmpty)
      lastPage getOrElse error("No page")
    else {
      val p = CurrentPage.is
      lastPage = Some(p)
      p
    }
}
class Page extends Observing {
  val id = randomString(20)
  def cometName = id
  def render = xml.Comment("comet " + id) ++ <lift:comet
    type="net.liftweb.reactive.ReactionsComet"
    name={cometName}
  />
}
