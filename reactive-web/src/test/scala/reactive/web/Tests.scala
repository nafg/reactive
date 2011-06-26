package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import net.liftweb.mockweb._

class RElemTests extends FunSuite with ShouldMatchers {
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
