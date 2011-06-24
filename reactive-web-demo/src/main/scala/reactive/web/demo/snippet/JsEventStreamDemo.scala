package reactive.web.demo.snippet

import reactive._
import web._
import javascript._
import JsTypes._

import net.liftweb.util._
import Helpers._

class JsEventStreamDemo extends Observing {
  val jses = new JsEventStream[JsString]

  val alert = JsIdent[JsFunction1[JsString,JsVoid]]('alert)
  
  def render = {
    Reactions.inServerScope(Page.currentPage) {
      jses.foreach { v: JsExp[JsString] =>
        alert.apply(v)
      }
    }
    "button" #> (DOMEventSource.click ->> jses.fire("Button clicked".j))
  }
}
