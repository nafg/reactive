package reactive.web

import scala.xml.NodeSeq

import net.liftweb.util.CanBind
import net.liftweb.util.Helpers.StringToCssBindPromoter
import reactive.{Observing, Signal, Var}

import org.scalatest.{FunSuite, Matchers}


class PropertyVarTests extends FunSuite with Matchers {
  // compile time test that the implicits resolve successfully
  { implicit page: Page =>
    implicitly[CanBind[Signal[NodeSeq => NodeSeq]]]
    implicitly[CanBind[Var[NodeSeq => NodeSeq]]]
    implicitly[CanBind[PropertyVar[String]]]
  }

  test("Emulate property change") {
    implicit object o extends Observing
    val value = PropertyVar("value")("initial") withEvents DomEventSource.change
    val page = new TestPage({ implicit p =>
      <html>{
        "#id" #> value apply <input id="id"/>
      }</html>
    })
    val ttt = page.testTransportType

    (ttt.xml \! "#id").value shouldBe "initial"

    println(ttt.xml)
    ttt.fire(ttt(ttt.xml \! "#id", "value") = "newValue", Change())
    value.now shouldBe "newValue"

    value ()= "anotherValue"
    (ttt.xml \! "#id").value shouldBe "anotherValue"
  }
}
