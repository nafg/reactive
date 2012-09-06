package reactive.web.html.widgets

import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.mockweb.MockWeb
import net.liftweb.util._
import net.liftweb.util.Helpers._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import reactive._
import reactive.web._
import reactive.web.html._
import reactive.web.javascript._
import reactive.web.javascript.JsTypes._
import scala.xml.{ Elem, Node, NodeSeq }

class AutoCompleteTest extends FunSuite with ShouldMatchers with Observing {
  //  implicit val config = Config.defaults

  test("The autocomplete's repeater's children should have the same ids as their representations " +
    "in the HTML Dom") {
    MockWeb.testS("/") {
      Page.withPage(new Page) {
        val template = <div><input id="theinput"/></div>
        val autocomplete = Autocomplete[String](
          (str: String) =>
            List("toto", "tutu") filter (_ startsWith str),
          (str: String) => str)
        def snippet: NodeSeq => NodeSeq = "#theinput" #> autocomplete
        val ts = new TestScope(snippet(template))
        import ts._
        Reactions.inScope(ts) {
          val domInput = autocomplete.input.render
          autocomplete.input.value() = "t"
          domInput fire KeyUp(40, new Modifiers)
          // println(autocomplete.repeat.children.now.head.events.head.propagateJS)
          // autocomplete.input.value() = "to"
          // println(autocomplete.repeat.children.now.head.focus().render)
          val rChildrenIds = autocomplete.repeat.children.now map (_.id) toList
          val divIds = (xml \\ "div" collect {
            case n @ _ if (n \ "@class" text) ==
              "reactive-autocomplete-candidate" => (n \ "@id" text)
          })
          println(xml)
          rChildrenIds should equal (divIds)
        }
      }
    }
  }
}
