package reactive
package web

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DomPropertyTests extends AnyFunSuite with Matchers {
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
      <html>{ prop1.render apply <span id="elem1" /> }{ prop2.render apply <span id="elem2" /> }</html>
    })

    val ttA = pageA.testTransportType
    val ttB = pageB.testTransportType

    def getPropUpdates(ttt: TestTransportType)(f: =>Any): Set[(String, Any)] = ttt.collectQueued(f).collect {
      case DomMutationRenderable(DomMutation.UpdateProperty(id, "prop", "prop", v)) => (id, v)
    }.toSet

    getPropUpdates(ttA) {
      getPropUpdates(ttB) {
        ttA.fire(ttA(ttA.xml \\! "#elem1", "prop") = "value1", Change())
      } shouldBe Set(("elem1", "value1"), ("elem2", "value1"))
    } shouldBe Set(("elem2", "value1"))

    ttA.xml \\! "#elem1" attr "prop" should equal("value1")
    ttA.xml \\! "#elem2" attr "prop" should equal("value1")

    ttB.xml \\! "#elem2" attr "prop" should equal("value1")
    ttB.xml \\! "#elem1" attr "prop" should equal("value1")

    getPropUpdates(ttA) {
      getPropUpdates(ttB) {
        ttB.fire(ttB(ttB.xml \\! "#elem1", "prop") = "value2", Change())
      } shouldBe Set(("elem2", "value2"))
    } shouldBe Set(("elem1", "value2"), ("elem2", "value2"))

    ttB.xml \\! "#elem1" attr "prop" should equal("value2")
    ttB.xml \\! "#elem2" attr "prop" should equal("value2")

    ttA.xml \\! "#elem1" attr "prop" should equal("value2")
    ttA.xml \\! "#elem2" attr "prop" should equal("value2")
  }
}
