package net.liftweb.reactive

import net.liftweb.util.Helpers.randomString
import net.liftweb.http.RequestVar

import reactive._

object CurrentPage extends RequestVar(new Page)

trait ReactiveSnippet {
  implicit def currentPage = CurrentPage.is
}
class Page extends Observing {
  val id = randomString(20)
  def cometName = id
  def render = <lift:comet
    type="net.liftweb.reactive.ReactionsComet"
    name={cometName}
  />
}
