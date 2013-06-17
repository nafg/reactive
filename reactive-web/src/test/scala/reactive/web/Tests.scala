package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.mockweb._
import scala.xml.{ Elem, NodeSeq, Text, UnprefixedAttribute, Null }
import org.scalatest.concurrent.Conductor
import java.lang.Thread
import scala.concurrent.SyncVar
import net.liftweb.util.Helpers._
import org.scalatest.prop.PropertyChecks
import scala.xml.Node
import org.scalatest.prop.TableDrivenPropertyChecks

class RElemTests extends FunSuite with ShouldMatchers {
  test("Rendering an RElem to an Elem with an id should retain that id") {
    implicit val page = new TestPage
    val elem = <anElem id="anId"/>
    val rElem = RElem(<span>A span</span>)
    rElem(elem).asInstanceOf[Elem].attribute("id").map(_.text) should equal(Some("anId"))
  }
}
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

//TODO split tests into separate files
class DomPropertyTests extends FunSuite with ShouldMatchers {
  test("DomProperty has one id per page") {
    implicit val page = new TestPage
    val property = DomProperty("someName")
    val e1 = property.render apply <elem1/>
    val e2 = property.render apply <elem2/>
    e1.attributes("id") should equal(e2.attributes("id"))
  }

  test("DomProperty updates go everywhere except same property on same page") {
    implicit val o = new Observing { }
    val prop1, prop2 = DomProperty("prop") withEvents DomEventSource.change
    prop1.values >> prop2
    val pageA, pageB = new TestPage({ implicit page =>
      <html>{ prop1.render apply <elem1/> }{ prop2.render apply <elem2/> }</html>
    })

    val ttA = pageA.testTransport
    ttA.fire(ttA(ttA.xml \\! "elem1", "prop") = "value1", Change())
    ttA.xml \\! "elem1" attr "prop" should equal("value1")
    ttA.xml \\! "elem2" attr "prop" should equal("value1")

    val ttB = pageB.testTransport
    ttB.xml \\! "elem2" attr "prop" should equal("value1")
    ttB.xml \\! "elem1" attr "prop" should equal("value1")

    ttB.fire(ttB(ttB.xml \\! "elem1", "prop") = "value2", Change())
    ttB.xml \\! "elem1" attr "prop" should equal("value2")
    ttB.xml \\! "elem2" attr "prop" should equal("value2")

    ttA.xml \\! "elem1" attr "prop" should equal("value2")
    ttA.xml \\! "elem2" attr "prop" should equal("value2")
  }
}

class DomEventSourceTests extends FunSuite with ShouldMatchers {
  test("DomEventSource only renders the current Page's propagation javascript") {
    MockWeb.testS("/") {
      val property = DomProperty("someName") withEvents DomEventSource.click
      implicit val page = new TestPage
      val e1 = property.render apply <elem1/>
      val e2 = property.render apply <elem1/>
      ((e1 \ "@onclick").text.split(";").length) should equal(3)
      ((e2 \ "@onclick").text.split(";").length) should equal(3)
    }
  }
}

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

class DomMutationTests extends FunSuite with ShouldMatchers with TableDrivenPropertyChecks {
  import DomMutation._

  test("Apply to xml") {
    val template = <elem><parent id="parentId"><child1 id="child1"/><child2 id="child2"/></parent></elem>
    val zipper = NodeLoc(template)

    val ops = Table(
      ("DomMutation", "Expected result"),
      (
        InsertChildBefore("parentId", <child3/>, "child2"),
        <elem><parent id="parentId"><child1 id="child1"/><child3/><child2 id="child2"/></parent></elem>
      ),
      (
        AppendChild("parentId", <child3/>),
        <elem><parent id="parentId"><child1 id="child1"/><child2 id="child2"/><child3/></parent></elem>
      ),
      (
        RemoveChild("parentId", "child1"),
        <elem><parent id="parentId"><child2 id="child2"/></parent></elem>
      ),
      (
        ReplaceChild("parentId", <child3/>, "child1"),
        <elem><parent id="parentId"><child3/><child2 id="child2"/></parent></elem>
      ),
      (
        ReplaceAll("parentId", <child3/> ++ <child4/>),
        <elem><parent id="parentId"><child3/><child4/></parent></elem>
      ),
      (
        UpdateProperty("parentId", "value", "value", 30),
        <elem><parent value="30" id="parentId"><child1 id="child1"/><child2 id="child2"/></parent></elem>
      )
    )

    forAll(ops) { (dm, exp) =>
      dm apply template should equal (exp)
      zipper.applyDomMutation(dm).top.node should equal (exp)
    }
  }

  test("Rendering") {
    defaultDomMutationRenderer(InsertChildBefore("parentId", <elem/>, "beforeId")) should equal (
      """reactive.insertChild('parentId',reactive.createElem('elem',{},""),'beforeId');"""
    )
  }
}
