package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import net.liftweb.mockweb._

class RElemTests extends FunSuite with ShouldMatchers {
  test("Rendering an RElem to an Elem with an id should retain that id") {
    Page.withPage(new Page) {
      val elem = <anElem id="anId"/>
      val rElem = RElem(<span>A span</span>)
      rElem(elem).asInstanceOf[scala.xml.Elem].attribute("id").map(_.text) should equal (Some("anId"))
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
