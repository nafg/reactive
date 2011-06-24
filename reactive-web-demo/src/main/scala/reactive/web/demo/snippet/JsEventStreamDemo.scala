package reactive.web.demo.snippet

import reactive._
import web._
import javascript._
import JsTypes._

import net.liftweb.util._
import Helpers._

class JsEventStreamDemo extends Observing {
  val jses = new JsEventStream[JsString]

  val window = $[JsObj]('window)
  val alert = $[JsString =|> JsVoid]('alert)

  def render = {
    Reactions.inServerScope(Page.currentPage) {
      jses.foreach { v: $[JsString] =>
        window->alert("Fired: ".$ + v)
      } 
    }
    "button" #> (DOMEventSource.click ->> jses.fire("Button clicked"$))
  }
}
