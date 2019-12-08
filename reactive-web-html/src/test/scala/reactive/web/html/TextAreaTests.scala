package reactive.web.html

import net.liftweb.util.Helpers.StringToCssBindPromoter
import reactive.Observing
import reactive.web.TestPage
import reactive.web.Change
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TextAreaTests extends AnyFunSuite with Matchers {
  test("TextArea") {
    implicit object obs extends Observing

    val textArea = TextArea("initial")

    val page = new TestPage({ implicit p =>
      <html>{
        "#id" #> textArea apply <textarea id="id"/>
      }</html>
    })

    val ttt = page.testTransportType

    (ttt.xml \! "#id").node.text shouldBe "initial"

    ttt.fire(ttt(ttt.xml \! "#id", "value") = "newValue", Change())
    textArea.value.now shouldBe "newValue"

    textArea.value ()= "anotherValue"
    (ttt.xml \! "#id").node.text shouldBe "anotherValue"
  }
}
