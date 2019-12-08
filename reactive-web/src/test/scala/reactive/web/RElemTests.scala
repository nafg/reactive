package reactive
package web


import scala.xml.Elem
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RElemTests extends AnyFunSuite with Matchers {
  test("Rendering an RElem to an Elem with an id should retain that id") {
    implicit val page = new TestPage
    val elem = <anElem id="anId"/>
    val rElem = RElem(<span>A span</span>)
    rElem(elem).asInstanceOf[Elem].attribute("id").map(_.text) should equal(Some("anId"))
  }
}
