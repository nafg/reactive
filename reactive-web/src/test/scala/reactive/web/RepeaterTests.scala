package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks

import scala.xml.{ Elem, NodeSeq }

import net.liftweb.mockweb._
import net.liftweb.util.Helpers._

class RepeaterTests extends FunSuite with ShouldMatchers with PropertyChecks {
  test("Repeater should render its children") {
    implicit val page = new TestPage
    val select = html.Select(Val(List(1, 2, 3)))(new Observing {}, Config.defaults)
    select(<select/>).asInstanceOf[Elem].child.length should equal(3)
  }

  test("Repeater should send correct deltas") {
    MockWeb.testS("/") {
      val template = <div><span></span></div>
      import org.scalacheck.Gen._
      forAll(listOf1(listOf(alphaUpperChar map (_.toString))), maxSize(10)) { xss: List[List[String]] =>
        whenever(xss.length >= 2) {
          val signal = BufferSignal(xss.head: _*)
          implicit val page = new TestPage({ implicit page =>
            def snippet: NodeSeq => NodeSeq = "div" #> Repeater(signal.now.map(x => "span *" #> x).signal)
            <html>{ snippet(template) }</html>
          })
          // println(Console.BLUE+"\n"+"=" * 25)
          // println(Console.BLUE+"Testing with: "+xss)
          // println(Console.BLUE+"js: "+page.testTransport.js)
          // println(Console.BLUE+"xml: "+page.testTransport.xml)
          for (xs <- xss.tail) {
            try {
              // println(Console.BLUE+"Setting to "+xs)
              signal () = xs
              // println(Console.BLUE+"js: "+page.testTransport.js)
              // println(Console.BLUE+"xml: "+page.testTransport.xml)
              (page.testTransport.xml \\ "span").length should equal (signal.now.length)
              (page.testTransport.xml \\ "span" map (_.node.text)).toSeq should equal (signal.now)
            } catch {
              case e: Exception =>
                println(Console.RED + e)
                e.getStackTrace.take(30) foreach { x => println(Console.RED + x.toString) }
                println(Console.RED + "X" * 25 + Console.RESET)
                throw e
            }
          }
          // println(Console.GREEN+"=" * 10+" Ok "+"=" * 11 + Console.RESET)
        }
      }
    }
  }
}
