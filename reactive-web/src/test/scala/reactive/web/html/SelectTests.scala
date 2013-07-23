package reactive
package web
package html

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class SelectTests extends FunSuite with ShouldMatchers with Observing {
  test("Selection should initally be defined") {
    val select = Select(Val(List("A", "B")))
    select.selectedIndex.now.isDefined should be(true)
  }

  test("Creating an empty Select should not IndexOutOfBounds") {
    Select(Val(Nil))
  }

  test("When selectedIndex is None then selectedItem is None") {
    val select = Select(Val(List("A", "B")))
    select.selectedIndex() = None
    select.selectedItem.now should equal(None)
  }

  test("Replacing items should cause selectedItem to change") {
    val items = Var(List("A", "B"))
    val select = Select(items)
    implicit val page = new TestPage({ implicit page =>
      select.render
    })

    select.items.now should equal(items.now)
    select.selectedItem.now should equal(Some("A"))
    select.selectedIndex.now should equal(Some(0))

    items() = List("C", "D")
    select.items.now should equal(items.now)
    select.selectedItem.now should equal(Some("C"))
    select.selectedIndex.now should equal(Some(0))

    items() = List("E", "F")
    select.items.now should equal(items.now)
    select.selectedItem.now should equal(Some("E"))
    select.selectedIndex.now should equal(Some(0))

    items() = List("E", "F")
    select.items.now should equal(items.now)
    select.selectedItem.now should equal(Some("E"))
    select.selectedIndex.now should equal(Some(0))
  }

  test("Replacing items maintains the correct selection") {
    val itemsA = List("N", "B", "T")
    val itemsB = List("N", "B", "T", "K")
    val v = Var(itemsA)
    val select = Select(v)
    select.selectedItem() = Some("N")
    v() = itemsB
    select.selectedItem.now should equal(Some("N"))
  }

  test("Even when rendering (calling addPage) twice, listeners are not called more than twice") {
    val select = Select(Val(List("A", "B", "C")))
    implicit val page = new TestPage({ implicit self =>
      select.render
      select.render
    })
    var itemChanges, indexChanges = 0
    select.selectedItem.change ->> { itemChanges += 1 }
    select.selectedIndex.change ->> { indexChanges += 1 }
    select.selectedItem() = Some("C")
    itemChanges should (be <= (2) and be >= (1))
    indexChanges should equal(1)
    select.selectedIndex() = Some(1)
    itemChanges should (be <= (3) and be >= (2))
    indexChanges should (be <= (3) and be >= (2))
  }
}
