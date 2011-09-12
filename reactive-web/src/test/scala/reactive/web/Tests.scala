package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.mockweb._

import scala.xml.{ Elem, NodeSeq, Text, UnprefixedAttribute, Null }

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

class TestScopeTests extends FunSuite with ShouldMatchers with Observing {
  import net.liftweb.util.Helpers._
  test("TestScope") {
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

  test("Emulate event") {
    Page.withPage(new Page) {
      var fired = false
      val event = DOMEventSource.keyUp ->> { fired = true }
      val input = event.render apply <input/>
      val ts = new TestScope(input)
      import ts._
      input fire KeyUp(56)
      fired should equal (true)
    }
  }

  test("Emulate property change") {
    Page.withPage(new Page) {
      val value = PropertyVar("value")("initial") withEvents DOMEventSource.change
      val input = value render <input id="id"/>
      val ts = new TestScope(input)
      import ts._
      (input("value") = "newValue") fire Change()
      value.now should equal ("newValue")
    }
  }
}

class DomMutationTests extends FunSuite with ShouldMatchers {
  import DomMutation._

  test("Apply to xml") {
    val template = <elem><parent id="parentId"><child1 id="child1"/><child2 id="child2"/></parent></elem>
    InsertChildBefore("parentId", <child3/>, "child2") apply template should equal (
      <elem><parent id="parentId"><child1 id="child1"/><child3/><child2 id="child2"/></parent></elem>
    )
    AppendChild("parentId", <child3/>) apply template should equal (
      <elem><parent id="parentId"><child1 id="child1"/><child2 id="child2"/><child3/></parent></elem>
    )
    RemoveChild("parentId", "child1") apply template should equal (
      <elem><parent id="parentId"><child2 id="child2"/></parent></elem>
    )
    ReplaceChild("parentId", <child3/>, "child1") apply template should equal (
      <elem><parent id="parentId"><child3/><child2 id="child2"/></parent></elem>
    )
    ReplaceAll("parentId", <child3/> ++ <child4/>) apply template should equal (
      <elem><parent id="parentId"><child3/><child4/></parent></elem>
    )
    val up = UpdateProperty("parentId", "value", "value", 30) apply template
    up.asInstanceOf[Elem].child(0).attributes.asAttrMap should equal (Map("id" -> "parentId", "value" -> "30"))
  }

  test("Rendering") {
    defaultDomMutationRenderer(InsertChildBefore("parentId", <elem/>, "beforeId")) should equal (
      """reactive.insertChild('parentId',reactive.createElem('elem',{},""),'beforeId')"""
    )
  }
}
