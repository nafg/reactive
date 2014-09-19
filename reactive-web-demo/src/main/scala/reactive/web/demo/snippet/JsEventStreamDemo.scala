package reactive
package web
package demo
package snippet

import reactive.web.javascript._
import JsTypes._

import net.liftweb.util._
import Helpers._

class JsEventStreamDemo extends PageSnippet {
  val clicks = DomEventSource.click
  // map all event values to a single string
  val jses = {
    implicit val stack = new JsStatementStack
    val ret = clicks.jsEventStream.map{ (_: $[JsObj]) => "Button clicked".$ }
    //this function will be executed in plain javascript, with no ajax involved!
    ret.foreach { v: JsExp[JsString] => window.alert("Fired: ".$ + v) }
    ret
  }


  //alert from the server too!
  jses.toServer[String] foreach { v => Javascript { implicit stack => window.alert("Server says: '"+v.toString+"'") } }

  def render =
    "button" #> clicks
}
