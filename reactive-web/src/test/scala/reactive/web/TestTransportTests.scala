package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scala.xml.{ NodeSeq, Text }

import net.liftweb.util.Helpers._
import net.liftweb.mockweb._


class TestTransportTests extends FunSuite with ShouldMatchers with Observing {
  test("TestTransport") {
    MockWeb.testS("/") {
      val template = <span id="span">A</span>
      val signal = Var("A")
      implicit val page = new Page { }
      def snippet: NodeSeq => NodeSeq =
        "span" #> Cell { signal map { s => { ns: NodeSeq => Text(s) } } }
      val tt = new TestTransport(<html>{ snippet apply template }</html>)(page)
      page.withTransport(tt) {
        signal() = "B"
      }
      (tt.xml \\! "span").node.text should equal ("B")
      tt.xml.node should equal (<html><span id="span">B</span></html>)
    }
  }

  test("Emulate event") {
    var fired = false
    val page = new TestPage({ implicit page =>
      val event = DomEventSource.keyUp ->> { fired = true }
      event.render apply <input/>
    })
    page.testTransport.fire(page.testTransport.xml, KeyUp(56))
    fired should equal (true)
  }

  test("Emulate property change") {
    val value = PropertyVar("value")("initial") withEvents DomEventSource.change
    val page = new TestPage({ implicit p =>
      value render <input id="id"/>
    })
    val tt = page.testTransport
    tt.fire(tt(tt.xml, "value") = "newValue", Change())
    value.now should equal ("newValue")
  }

  test("Confirm") {
    implicit val page = new TestPage
    var result: Option[Boolean] = None
    confirm("Are you sure?") { case b => result = Some(b) }
    page.testTransport.takeConfirm match {
      case Some((msg, f)) =>
        msg should equal("Are you sure?")
        f(true)
        result should equal(Some(true))
        f(false)
        result should equal(Some(false))
      case _ =>
        fail("No confirm function found.")
    }
  }
}
