package reactive.web.demo.snippet

import reactive._
import web._
import javascript._
import JsTypes._

import net.liftweb.util._
import Helpers._

class JsEventStreamDemo extends Observing {
  trait window extends JsStub {
    def alert(s: $[JsString]): $[JsVoid]
  }
  val window = $$[window]

  val clicks = DOMEventSource.click
  val jses = clicks.jsEventStream.map{ (_: $[JsObj]) => "Button clicked"$ }

  //this function will be executed in plain javascript, with no ajax involved!
  jses.foreach { v: $[JsString] => window.alert("Fired: ".$ + v) }

  //alert from the server too!
  jses.toServer[String] foreach { v => reactive.web.alert("Server says: '"+v.toString+"'") }

  def render =
    "button" #> clicks
}
