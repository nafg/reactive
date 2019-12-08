package reactive
package web

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DomEventSourceTests extends AnyFunSuite with Matchers {
  test("DomEventSource only renders the current Page's propagation javascript") {
    val property = DomProperty("someName") withEvents DomEventSource.click
    implicit val page = new TestPage
    val e1 = property.render apply <elem1/>
    val e2 = property.render apply <elem1/>
    (e1 \ "@onclick").text.split(";").length should equal(3)
    (e2 \ "@onclick").text.split(";").length should equal(3)
  }
}
