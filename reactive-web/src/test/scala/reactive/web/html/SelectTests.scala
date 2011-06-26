package reactive
package web
package html

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class SelectTests extends FunSuite with ShouldMatchers with Observing {
  def withNewPage[T](p: => T): T = Page.withPage(new Page)(p)

  test("Selection should initally be defined") {
    val select = withNewPage(Select(Val(List("A", "B"))))
    select.selectedIndex.now.isDefined should be(true)
  }

  test("Creating an empty Select should not IndexOutOfBounds") {
    withNewPage(Select(Val(Nil)))
  }

  test("When selectedIndex is None then selectedItem is None") {
    val select = withNewPage(Select(Val(List("A", "B"))))
    select.selectedIndex() = None
    select.selectedItem.now should equal(None)
  }

  test("Replacing items should cause selectedItem to change") {
    val items = Var(List("A", "B"))
    val select = withNewPage(Select(items))

    //    select.selectedItem foreach println
    //    select.selectedIndex foreach println

    select.selectedItem.now should equal(Some("A"))
    println("Updating")

    items() = List("C", "D")
    select.selectedItem.now should equal(Some("C"))
    select.selectedIndex.now should equal(Some(0))

    items() = List("E", "F")
    select.selectedItem.now should equal(Some("E"))
    select.selectedIndex.now should equal(Some(0))
  }

}
