package reactive
package web
package javascript

import JsTypes._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class JsTests extends FunSuite with ShouldMatchers {
  test("Operators") {
    (1.$ + 2 render) should equal (new JsOp(1, 2, "+").render)
    (1.$ & 2 render) should equal (new JsOp(1, 2, "&").render)
    (1.$ | 2 render) should equal (new JsOp(1, 2, "|").render)
    (true.$ || false render) should equal (new JsOp(true, false, "||").render)
    (1.$ === 2 render) should equal (new JsOp(1, 2, "==").render)
    (1.$ !== 2 render) should equal (new JsOp(1, 2, "!=").render)

    (!(true.$) render) should equal ("(!true)")
  }

  test("Functions") {
    { x: $[JsNumber] => x + 1.$ }.$.render should equal ("(function(arg){return (arg+1)})")

    { x: $[JsNumber] =>
      If(x > 10) {
        window alert "Greater"
      } Else {
        window alert "Small"
      }
    }.$.render should equal (
      "(function(arg){if((arg>10)) {window.alert(\"Greater\")} else {window.alert(\"Small\")};return })"
    )
  }

  test("JsStub") {
    sealed trait obj extends JsStub {
      def method(s: $[JsString]): $[JsString]
      var self: obj
      def nullary: $[JsVoid]
      def prop: Assignable[JsNumber]
      def takeCallback(fn: JsExp[JsTypes.JsVoid =|> JsTypes.JsAny]): JsExp[JsTypes.JsNumber]
      def getSelf(i: $[JsNumber]): obj
    }
    trait extendObj extends JsStub {
      def takeCallback2(fn: JsExp[JsTypes.JsVoid =|> JsTypes.JsAny]): JsExp[JsTypes.JsNumber]
      def getSelf2(i: $[JsNumber]): obj
    }
    implicit object ext extends Extend[obj, extendObj]
    val obj = $$[obj]
    Page.withPage(new Page) {
      Reactions.inScope(new LocalScope) {
        Javascript {
          obj.method(obj.method("This is a scala string"))
          val v = JsVar[JsObj] := obj.self
          obj.nullary
          obj.prop := 2
          obj.get("otherProp") := "xyz"
          obj.getSelf(1).getSelf(2)
          obj.takeCallback{ _: JsExp[JsTypes.JsVoid] =>
            obj.takeCallback2{ _: JsExp[JsTypes.JsVoid] =>
              obj.getSelf(1).getSelf2(2).getSelf(3).getSelf2(4)
            }
          }
        }
      }.js.map(_.toJsCmd) zipAll (List(
        "obj.method(obj.method(\"This is a scala string\"))",
        "var x$0",
        "x$0=obj.self",
        "obj.nullary()",
        "obj.prop=2",
        """obj["otherProp"]="xyz"""",
        "obj.getSelf(1).getSelf(2)",
        "obj.takeCallback((function(arg){return obj.takeCallback2((function(arg){return obj.getSelf(1).getSelf2(2).getSelf(3).getSelf2(4)}))}))"
      ), "", "") foreach { case (a, b) => a should equal (b) }
    }
  }

  test("JsStub+Extend"){
    import JsTypes._
    trait JQuery extends JsStub
    trait JQueryElem extends JsStub {
      def bind(eventName: JsExp[JsString])(handler: JsExp[JsTypes.JsObj =|> JsTypes.JsAny]): JQueryElem
    }
    trait Window2 extends JsStub {
      def jQuery(sel: JsExp[JsTypes.JsString]): JQueryElem
      def jQueryReady(fn: JsExp[JsTypes.JsVoid =|> JsTypes.JsAny]): JsExp[JsTypes.JsVoid]
    }
    trait JQueryJsTreeElem extends JsStub {
      def jstree(args: JsExp[JsTypes.JsAny]*): JQueryElem
    }
    implicit object extendWindow extends Extend[Window, Window2]
    implicit object jqElem2jqJstree extends Extend[JQueryElem, JQueryJsTreeElem]

    Page.withPage(new Page) {
      val res = Reactions.inScope(new LocalScope) {
        Javascript {
          window.jQueryReady{ _: JsExp[JsTypes.JsVoid] =>
            window.setTimeout({ _: JsExp[JsTypes.JsVoid] =>
              //TODO not chained
              window.jQuery(".items").jstree("create",
                JsRaw[JsTypes.JsString](null),
                "last"
              ).bind("rename.jstree"){ _: JsExp[JsObj] =>
                  Ajax{ x: Int => println("Got rename "+x+"!") } apply 10
                }
            }, 1000)
          }
        }
      }.js.map(_.toJsCmd)
      res foreach println
      res.length should equal (1)
      res zipAll (List(
        "window.jQueryReady("+
          "(function(arg){"+
          "return window.setTimeout("+
          "(function(arg){"+
          "return window.jQuery(\".items\").jstree(\"create\",null,\"last\").bind("+
          "\"rename.jstree\","+
          "(function(arg){(function(arg){reactive.queueAjax(0)(arg);reactive.doAjax()})(10);return })"+
          ")"+"}),"+
          "1000"+
          ")"+
          "})"+
          ")"
      ), "", "") foreach { case (a, b) => a should equal (b) }
    }
  }

  test("Function bodies do not get repeated") {
    def isClean = window.get("isClean").asInstanceOf[Assignable[JsTypes.JsBoolean]]
    val stmts = JsStatement.inScope{
      // This does not create a  statement
      { e: $[JsTypes.JsObj] =>
        Return()
      }: JsExp[JsObj =|> JsAny]
      // This does
      window.onbeforeunload := { e: $[JsTypes.JsObj] =>
        If(!isClean) {
          object reply extends JsVar[JsTypes.JsString]
          reply := "You have unsaved changes!"
          object evt extends JsVar[JsTypes.JsObj]
          evt := e
          If (evt === null) { evt := window.event }
          If (evt !== null) { evt.get("returnValue") := reply }
          Return(reply)
        }
      }
    }._2.map(JsStatement.render)
    stmts foreach println
    stmts.zipAll(List(
      """window.onbeforeunload=(function(arg){if((!window["isClean"])) {var reply;var evt;reply="You have unsaved changes!";evt=arg;if((evt==null)) {evt=window.event};if((evt!=null)) {evt["returnValue"]=reply};return reply};return })"""
    ), "", "") foreach { case (a, b) => a should equal (b) }
  }

  test("Statements") {
    window.alert(window.encodeURIComponent("Message"))
    JsStatement.render(JsStatement.pop) should equal ("window.alert(window.encodeURIComponent(\"Message\"))")

    val (_, theStatements) = JsStatement.inScope{
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

        val myAjax = Ajax{ x: String => println("Got "+x) }
        myAjax("Hello server!")
      }
    }
    val rendered = theStatements map JsStatement.render
    //    theStatements map JsStatement.render foreach println
    val target = List(
      """if(true) {window.alert("True")} else if(false) {window.alert("False")} else {if(true) {} else {}}""",
      """while(true) {window.alert("Again!")}""",
      """do {window.alert("Hello!")} while(false)""",
      """switch(1) {case 0:window.alert("No");break;case 1:window.alert("Yes");break;}""",
      """var i""",
      """for(i=1;(i<10);i=(i+1)) {}""",
      """for(var x$0 in [1,2,3]) {if((x$0>1)) {window.alert("Greater")}}""",
      """for each(var x$1 in [1,2,3]) {if((x$1>1)) {window.alert("Greater")}}""",
      """try {throw "message"} catch(x$2) {} finally {}""",
      """function myFunc(arg0){if((arg0>10)) {window.alert("Greater")} else {window.alert("Small")}}""",
      "myFunc(10)",
      "function f$0(arg0){return (arg0>10)}",
      """(function(arg){reactive.queueAjax(1)(arg);reactive.doAjax()})("Hello server!")"""
    )
    rendered.length should equal (target.length)
    rendered.zipAll(target, "", "") foreach { case (s, t) => s should equal (t) }
  }
}
