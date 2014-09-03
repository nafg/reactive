package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scala.xml.{ Elem, NodeSeq }
import net.liftweb.util.Helpers._
import net.liftweb.util.CanBind

class RepeaterTests extends FunSuite with Matchers with PropertyChecks {
  // compile time test that the implicits resolve successfully
  { implicit page: Page =>
    implicitly[CanBind[SeqSignal[NodeSeq => NodeSeq]]]
    implicitly[CanBind[Signal[NodeSeq => NodeSeq]]]
  }

  test("Repeater should render its children") {
    implicit val page = new TestPage
    val select = html.Select(Val(List(1, 2, 3)))(new Observing {}, Config.defaults)
    select(<select/>).asInstanceOf[Elem].child.length should equal(3)
  }

  test("Repeater should send correct deltas") {
    val template = <div><span></span></div>
    import org.scalacheck.Gen._
    forAll(nonEmptyListOf(listOf(alphaUpperChar map (_.toString))), maxSize(10)) { xss: List[List[String]] =>
      whenever(xss.length >= 2) {
        val signal = BufferSignal(xss.head: _*)
        implicit val page = new TestPage({ implicit page =>
          def snippet: NodeSeq => NodeSeq = "div" #> signal.now.map(x => "span *" #> x).signal
          <html>{ snippet(template) }</html>
        })
        for (xs <- xss.tail) {
          try {
            signal () = xs
            (page.testTransportType.xml \\ "span").length should equal (signal.now.length)
            (page.testTransportType.xml \\ "span" map (_.node.text)).toSeq should equal (signal.now)
          } catch {
            case e: Exception =>
              println(Console.RED + e)
              e.getStackTrace.take(30) foreach { x => println(Console.RED + x.toString) }
              println(Console.RED + "X" * 25 + Console.RESET)
              throw e
          }
        }
      }
    }
  }
}
