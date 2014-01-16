package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

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

    val ttA = pageA.testTransportType
    ttA.fire(ttA(ttA.xml \\! "elem1", "prop") = "value1", Change())
    ttA.xml \\! "elem1" attr "prop" should equal("value1")
    ttA.xml \\! "elem2" attr "prop" should equal("value1")

    val ttB = pageB.testTransportType
    ttB.xml \\! "elem2" attr "prop" should equal("value1")
    ttB.xml \\! "elem1" attr "prop" should equal("value1")

    ttB.fire(ttB(ttB.xml \\! "elem1", "prop") = "value2", Change())
    ttB.xml \\! "elem1" attr "prop" should equal("value2")
    ttB.xml \\! "elem2" attr "prop" should equal("value2")

    ttA.xml \\! "elem1" attr "prop" should equal("value2")
    ttA.xml \\! "elem2" attr "prop" should equal("value2")
  }
}
