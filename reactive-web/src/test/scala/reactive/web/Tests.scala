package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.mockweb._

import scala.xml.{ Elem, NodeSeq, Text }

class RElemTests extends FunSuite with ShouldMatchers {
  test("Rendering an RElem to an Elem with an id should retain that id") {
    Page.withPage(new Page) {
      val elem = <anElem id="anId"/>
      val rElem = RElem(<span>A span</span>)
      rElem(elem).asInstanceOf[Elem].attribute("id").map(_.text) should equal(Some("anId"))
    }
  }
}
class RepeaterTests extends FunSuite with ShouldMatchers {
  test("Repeater should have children with toNSFunc") {
    MockWeb.testS("/") {
      val select = html.Select(Val(List(1, 2, 3)))(new Observing {}, Config.defaults)
      select(<select/>).asInstanceOf[Elem].child.length should equal (3)
    }
  }
}

class DOMPropertyTests extends FunSuite with ShouldMatchers {
  test("DOMProperty has one id per page") {
    Page.withPage(new Page) {
      val property = DOMProperty("someName")
      val e1 = property.render apply <elem1/>
      val e2 = property.render apply <elem2/>
      e1.attributes("id") should equal(e2.attributes("id"))
    }
  }

}

class DOMEventSourceTests extends FunSuite with ShouldMatchers {
  test("DOMEventSource only renders the current Page's propagation javascript") {
    MockWeb.testS("/") {
      val property = DOMProperty("someName") withEvents DOMEventSource.click
      val e1 = Page.withPage(new Page)(property.render apply <elem1/>)
      val e2 = Page.withPage(new Page)(property.render apply <elem1/>)
      ((e1 \ "@onclick" text) split ";" length) should equal (3)
      ((e2 \ "@onclick" text) split ";" length) should equal (3)
    }
  }
}

class DomMutationTests extends FunSuite with ShouldMatchers {
  import net.liftweb.util.Helpers._
  test("Can simulate DomMutations") {
    MockWeb.testS("/") {
      val template = <span id="span">A</span>
      val signal = Var("A")
      def snippet: NodeSeq => NodeSeq =
        "span" #> Cell{ signal map { s => { ns: NodeSeq => Text(s) } } }
      val xml = Reactions.inScope(new TestScope(snippet apply template)) {
        signal () = "B"
      }.xml
      (xml \\ "span" text) should equal ("B")
      xml.toString should equal (<span id="span">B</span> toString)
    }
  }

  test("Rendering") {
    DomMutation.
      defaultDomMutationRenderer(DomMutation.InsertChildBefore("parentId", <elem/>, "beforeId")) should equal (
        """reactive.insertChild('parentId',reactive.createElem('elem',{},""),'beforeId')"""
      )
  }
}
