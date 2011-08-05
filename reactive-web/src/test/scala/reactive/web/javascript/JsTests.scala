package reactive
package web
package javascript

import JsTypes._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class JsTests extends FunSuite with ShouldMatchers {
  test("Operators") {
    (1.$ + 2.$ render) should equal (new JsOp(1.$, 2.$, "+").render)
    (1.$ & 2.$ render) should equal (new JsOp(1.$, 2.$, "&").render)
    (1.$ | 2.$ render) should equal (new JsOp(1.$, 2.$, "|").render)
    (true.$ || false.$ render) should equal (new JsOp(true.$, false.$, "||").render)
    (1.$ === 2.$ render) should equal (new JsOp(1.$, 2.$, "==").render)
    (1.$ !== 2.$ render) should equal (new JsOp(1.$, 2.$, "!=").render)

    (!(true.$) render) should equal ("(!true)")
  }
  test("Statements") {
    window.alert(window.encodeURIComponent("Message"$))
    JsStatement.pop.render should equal ("window.alert(window.encodeURIComponent(\"Message\"))")

    Reactions.inScope(new Reactions.LocalScope) {
      window.alert("This is a scala string")
    }.js.map(_.toJsCmd) should equal (List("window.alert(\"This is a scala string\")"))
  }
}
