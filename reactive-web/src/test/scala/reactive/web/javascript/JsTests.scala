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
    JsStatement.render(JsStatement.pop) should equal ("window.alert(window.encodeURIComponent(\"Message\"))")

    Reactions.inScope(new Reactions.LocalScope) {
      window.alert("This is a scala string")
    }.js.map(_.toJsCmd) should equal (List("window.alert(\"This is a scala string\")"))

    val theStatements = JsStatement.inScope{
      If(true.$) {
        window.alert("True"$)
      }.ElseIf (false.$){
        window.alert("False"$)
      } Else {
        If(true.$) {
        } Else {
        }
      }
      While(true.$) {
        window.alert("Again!"$)
      }
      Do {
        window.alert("Hello!"$)
      } While (false.$)
      Switch(1.$)(
        0.$ :> {
          window.alert("No"$)
        },
        1.$ :> window.alert("Yes"$)
      )
    }
    theStatements.map(JsStatement.render) should equal (List(
      """if(true) {window.alert("True")} else if(false) {window.alert("False")} else {if(true) {} else {}}""",
      """while(true) {window.alert("Again!")}""",
      """do {window.alert("Hello!")} while(false)""",
"""switch(1) {case 0: window.alert("No");
break;
case 1: window.alert("Yes");
break;}"""
    ))
  }
}
