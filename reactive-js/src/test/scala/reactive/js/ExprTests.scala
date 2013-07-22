package reactive.js

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import JsAst._

class ExprTests extends FunSuite with ShouldMatchers with JsTestUtils {
  test("binary operators") {
    js.javascript {
      val a = 1
      val b = 2
      val c = a + 3 * b
    } should equal (Block(List(
      Declare("a"),
      Assign(SimpleIdentifier("a"),LitNum(1)),
      Declare("b"),
      Assign(SimpleIdentifier("b"),LitNum(2)),
      Declare("c"),
      Assign(
        SimpleIdentifier("c"),
        BinOp(
          SimpleIdentifier("a"),
          "+",
          BinOp(LitNum(3),"*",SimpleIdentifier("b"))
        )
      )
    )))
  }

  test("function literals and application") {
    js.javascript {
      val a = { (i: Int, j: Int) =>
        window.console.log("hello")
        i * j
      }
      val b = a(10, 10)
    } should equal (Block(List(
      Declare("a"),
      Assign(
        SimpleIdentifier("a"),
        LitFunction(
          List("i", "j"),
          Block(List(
            Apply(winConLog, LitStr("hello")),
            Return(
              BinOp(SimpleIdentifier("i"),"*",SimpleIdentifier("j"))
            )
          ))
        )
      ),
      Declare("b"),
      Assign(
        SimpleIdentifier("b"),
        Apply(SimpleIdentifier("a"),LitNum(10),LitNum(10))
      )
    )))
  }

  test("object literal") {
    js.javascript {
      val a = js.Object(
        "x" -> 10,
        "y" -> "??",
        "z" -> ((i: Int) => i % 2 == 0)
      )
    } should equal (Block(List(
      Declare("a"),
      Assign(
        SimpleIdentifier("a"),
        LitObject(
          List(
            ("x",LitNum(10)),
            ("y",LitStr("??")),
            (
              "z",
              LitFunction(
                List("i"),
                Block(List(
                  Return(
                    BinOp(
                      BinOp(SimpleIdentifier("i"),"%",LitNum(2)),
                      "==",
                      LitNum(0)
                    )
                  )
                ))
              )
            )
          )
        )
      )
    )))
  }
}
