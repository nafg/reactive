package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class DomMutationTests extends FunSuite with Matchers with TableDrivenPropertyChecks {
  import DomMutation._

  test("Apply to xml") {
    val template = <elem><parent id="parentId"><child1 id="child1"/><child2 id="child2"/></parent></elem>
    val zipper = NodeLoc(template)

    val ops = Table(
      ("DomMutation", "Expected result"),
      (
        InsertChildBefore("parentId", <child3/>, "child2"),
        <elem><parent id="parentId"><child1 id="child1"/><child3/><child2 id="child2"/></parent></elem>
      ),
      (
        AppendChild("parentId", <child3/>),
        <elem><parent id="parentId"><child1 id="child1"/><child2 id="child2"/><child3/></parent></elem>
      ),
      (
        RemoveChild("parentId", "child1"),
        <elem><parent id="parentId"><child2 id="child2"/></parent></elem>
      ),
      (
        ReplaceChild("parentId", <child3/>, "child1"),
        <elem><parent id="parentId"><child3/><child2 id="child2"/></parent></elem>
      ),
      (
        ReplaceAll("parentId", <child3/> ++ <child4/>),
        <elem><parent id="parentId"><child3/><child4/></parent></elem>
      ),
      (
        UpdateProperty("parentId", "value", "value", 30),
        <elem><parent value="30" id="parentId"><child1 id="child1"/><child2 id="child2"/></parent></elem>
      )
    )

    forAll(ops) { (dm, exp) =>
      dm apply template should equal (exp)
      zipper.applyDomMutation(dm).top.node should equal (exp)
    }
  }

  test("Rendering") {
    DomMutationRenderer.default(InsertChildBefore("parentId", <elem/>, "beforeId")).render should equal (
      """reactive.insertChild('parentId',reactive.createElem('elem',{},""),'beforeId');"""
    )
  }
}
