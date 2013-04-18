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
  val jses = clicks.jsEventStream.map{ (_: $[JsObj]) => "Button clicked"$ }

  //this function will be executed in plain javascript, with no ajax involved!
  jses.foreach { v: $[JsString] => window.alert("Fired: ".$ + v) }

  //alert from the server too!
  jses.toServer[String] foreach { v => window.alert("Server says: '"+v.toString+"'") }

  def render =
    "button" #> clicks
}
