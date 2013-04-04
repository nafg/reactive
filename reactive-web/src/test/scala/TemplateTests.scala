package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scala.xml.NodeSeq

class TemplateTests extends FunSuite with ShouldMatchers {
  test("Test") {
    // Templates.printf("%s %d\n", "Hello", 32)

    val res = new Templates.Template[
      {def `/home/naftoli/dev/lrbcol/src/main/webapp/templates-hidden/default.html`: Unit}
    ]().apply(
      reactive = { ns: NodeSeq =>
        ns ++ <xml/>
      }
    )

    println(res)

/*
    Snippets("filepath")(
      SomeFunction = identity,
      SomeOtherFunction = identity
    ) ->

    val xml1 = ...
    val xml2 = replace(....)
*/
  }
}
