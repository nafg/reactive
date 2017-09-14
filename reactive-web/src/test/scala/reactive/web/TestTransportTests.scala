package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.Matchers

import scala.xml.{ NodeSeq, Text }

import net.liftweb.util.Helpers._

class TestTransportTests extends FunSuite with Matchers with Observing {

  test("TestTransport") {
    val template = <span id="span">A</span>
    val signal = Var("A")
    implicit val page = new TestPage({ implicit p =>
      def snippet: NodeSeq => NodeSeq =
        "span" #> Cell { signal map { s => { _: NodeSeq => Text(s) } } }
      <html>{ snippet apply template }</html>
    })

    signal() = "B"

    (page.testTransportType.xml \\! "span").node.text should equal ("B")
    page.testTransportType.xml.node should equal (<html><span id="span">B</span></html>)
  }

  test("Emulate event") {
    var fired = false
    val page = new TestPage({ implicit p =>
      val event = DomEventSource.keyUp ->> { fired = true }
      event.render apply <input/>
    })
    page.testTransportType.fire(page.testTransportType.xml, KeyUp(56))
    fired should equal (true)
  }

  test("Emulate property change") {
    val value = PropertyVar("value")("initial") withEvents DomEventSource.change
    val page = new TestPage({ implicit p =>
      value render <input id="id"/>
    })
    val tc = page.testTransportType
    tc.fire(tc(tc.xml, "value") = "newValue", Change())
    value.now should equal ("newValue")
  }

  test("Confirm") {
    implicit val page = new TestPage
    var result: Option[Boolean] = None
    confirm("Are you sure?") { case b => result = Some(b) }
    page.testTransportType.takeConfirm match {
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
