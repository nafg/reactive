package reactive
package web
package js

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scala.xml.NodeSeq
import JsAst._

object window extends js.Object {
  object console extends js.Object {
    def log(s: String): Unit = ???
  }
}

/**
 * adds the javascript code rendering to the
 * toString output (alongside the AST toString)
 * to aid in debugging "did not equal" test failures
 */
case class WithRender(s: JsAst.Statement) {
  override def toString = render(s) + " /*"+s.toString+"*/"
  def println: this.type = {
    Console.println(toString)
    this
  }
}

class JavascriptTests extends FunSuite with ShouldMatchers {
  val winConLog = Select(Select(SimpleIdentifier("window"), "console"), "log")

  implicit class printStatement(s: Statement) {
    def info: Statement = {
      JavascriptTests.this.info(withRender.toString)
      s
    }
    def withRender = WithRender(s)
  }

  JsAst.indent.value = Some(0)

  test("val") {
    js.javascript {
      val i = "hello"
    } should equal (Block(List(
      Declare("i"),
      Assign(SimpleIdentifier("i"), LitStr("hello"))
    )))
  }

  test("var") {
    js.javascript {
      var i = "hello"
      window.console.log(i)
      i = "goodbye"
      window.console.log(i)
    } should equal (Block(List(
      Declare("i"),
      Assign(SimpleIdentifier("i"), LitStr("hello")),
      Apply(winConLog, SimpleIdentifier("i")),
      Assign(SimpleIdentifier("i"), LitStr("goodbye")),
      Apply(winConLog, SimpleIdentifier("i"))
    )))
  }

  test("if") {
    js.javascript {
      if(true) window.console.log("true")
      else window.console log "false"
    } should equal (If(
      LitBool(true),
      Block(List(Apply(winConLog, LitStr("true")))),
      Block(List(Apply(winConLog, LitStr("false"))))
    ))
  }

  test("while") {
    js.javascript {
      while(true) {
        window.console.log("hi")
      }
    } should equal (While(
      LitBool(true),
      Block(List(Apply(winConLog, LitStr("hi"))))
    ))
  }

  test("do/while") {
    js.javascript {
      do {
        window.console.log("hi")
      } while(false)
    } should equal (DoWhile(
      Block(List(
        Apply(winConLog, LitStr("hi"))
      )),
      LitBool(false)
    ))
  }

  test("switch") {
    js.javascript {
      10 match {
        case 1 | 2  => window.console.log("1|2")
        case 10     => window.console.log("10")
        case _      => window.console.log("??");window.console.log("??")
      }
    } should equal (Switch(
      LitNum(10),
      List(
        Case(
          List(LitNum(1), LitNum(2)),
          List(Apply(winConLog,LitStr("1|2")))
        ),
        Case(
          List(LitNum(10)),
          List(Apply(winConLog,LitStr("10")))
        )
      ),
      Some(
        List(
          Apply(winConLog,LitStr("??")),
          Apply(winConLog,LitStr("??"))
        )
      )
    ))
  }
}
