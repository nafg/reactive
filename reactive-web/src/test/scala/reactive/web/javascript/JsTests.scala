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

  test("Functions") {
    { x: $[JsNumber] => x + 1.$ }.$.render should equal ("function(arg){return (arg+1)}")

    { x: $[JsNumber] =>
      If(x > 10) {
        window alert "Greater"
      } Else {
        window alert "Small"
      }
    }.$.render should equal (
      "function(arg){\n"+
        "  if((arg>10)) {window.alert(\"Greater\")} else {window.alert(\"Small\")}\n"+
        "}"
    )
  }

  test("JsStub") {
    sealed trait obj extends JsStub {
      def method(s: $[JsString]): $[JsString]
      def self: obj
    }
    val obj = $$[obj]
    Page.withPage(new Page) {
      Reactions.inScope(new LocalScope) {

        Javascript {
          obj.method(obj.method("This is a scala string"))
          val v = JsVar[JsObj] := obj.self
        }

      }.js.map(_.toJsCmd) should equal (List(

        "obj.method(obj.method(\"This is a scala string\"))",
        "var x$0",
        "x$0=obj.self"

      ))
    }
  }

  test("Statements") {
    window.alert(window.encodeURIComponent("Message"))
    JsStatement.render(JsStatement.pop) should equal ("window.alert(window.encodeURIComponent(\"Message\"))")

    val theStatements = JsStatement.inScope{
      If(true) {
        window.alert("True")
      }.ElseIf (false){
        window.alert("False")
      } Else {
        If(true) {
        } Else {
        }
      }
      While(true) {
        window.alert("Again!")
      }
      Do {
        window.alert("Hello!")
      } While (false)
      Switch(1)(
        0.$ :> {
          window.alert("No")
        },
        1.$ :> window.alert("Yes")
      )
      object i extends JsVar[JsNumber]
      For(List(i := 1), i < 10, List(i := i + 1)) {}

      Page.withPage(new Page){
        for (j <- List(1.$, 2.$, 3.$)$) {
          If(j > 1) {
            window.alert("Greater"$)
          }
        }
        for (j <- Each(List(1.$, 2.$, 3.$))) {
          If(j > 1) {
            window.alert("Greater")
          }
        }
        Try {
          Throw("message")
        } Catch { c =>
        } Finally {
        }
      }

      object myFunc extends Function({ x: $[JsNumber] =>
        If(x > 10) {
          window alert "Greater"
        } Else {
          window alert "Small"
        }
      })
      myFunc(10)
      Page.withPage(new Page) {
        val myFunc2 = Function({ x: $[JsNumber] => Return(x > 10) })
      }
    }
    theStatements.map(JsStatement.render) should equal (List(
      """if(true) {window.alert("True")} else if(false) {window.alert("False")} else {if(true) {} else {}}""",
      """while(true) {window.alert("Again!")}""",
      """do {window.alert("Hello!")} while(false)""",
      """switch(1) {case 0: window.alert("No");
break;
case 1: window.alert("Yes");
break;}""",
      """var i""",
      """for(i=1;(i<10);i=(i+1)) {}""",
      """for(var x$0 in [1,2,3]) {if((x$0>1)) {window.alert("Greater")}}""",
      """for each(var x$1 in [1,2,3]) {if((x$1>1)) {window.alert("Greater")}}""",
      """try {throw "message"} catch(x$2) {} finally {}""",
      "function myFunc(arg){\n"+
        "  if((arg>10)) {window.alert(\"Greater\")} else {window.alert(\"Small\")}\n"+
        "}",
      "myFunc(10)",
      "function f$0(arg){\n  return (arg>10)\n}"
    ))
  }
}
