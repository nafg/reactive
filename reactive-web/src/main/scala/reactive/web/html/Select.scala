package reactive
package web
package html

import scala.xml.Elem


/**
 * Represents a select element in the DOM.
 * @tparam T the type of item rendered by this select
 * @param items a SeqSignal containing the items rendered by this select
 * @param renderer a function that determines the representation of items. Defaults to calling _.toString
 * @param size the height of the select. Defaults to 1, i.e., a drop-down.
 */
class Select[T](
  items: SeqSignal[T],
  renderer: T=>String = {t:T => t.toString},
  val size: Int = 1
)(implicit observing: Observing) extends Repeater with Logger {
  case class SelectedIndexOutOfBounds(index: Int, length: Int) extends LogEventPredicate {
    override def toString = "selectedIndex %d is out of bounds (%d)".format(index,length)
  }
  
  /**
   * The change DOM event
   */
  val change = DOMEventSource.change
  /**
   * The click DOM event
   */
  val click = DOMEventSource.click
  /**
   * The selectedIndex DOM property
   * Also when the select is rendered, this affects which option has the selected="selected" attribute. 
   */
  val selectedIndex = Select.selectedIndex(None) updateOn change

  /**
   * A signal that represents the selected item as a T.
   */
  val selectedItem: Signal[Option[T]] = for {
    si <- selectedIndex
    is <- items
  } yield si.filter{i =>
    if(i < is.length && i >= 0) true else {warn(SelectedIndexOutOfBounds(i,is.length)); false}
  } map is

  /**
   * Call this to select another (or no) item.
   */
  //TODO perhaps just use a Var?
  //TODO what about multiple selections? Use another class?
  def selectItem(item: Option[T]) {
    selectedIndex ()= item.map(items.now.indexOf(_)).filter(_ != -1)
  }

  lazy val children = items.map {
    _ map {item: T =>
      RElem {
        if(selectedItem.now == Some(item))
          <option selected="selected">{renderer(item)}</option>
        else
          <option>{renderer(item)}</option>
      }
    }
  }

  override protected def addPage(elem: Elem)(implicit page: Page): Elem = {
    items.change.foreach{is =>
      val i = selectedIndex.now map {_.min(is.length-1)} filter {_ >= 0}
      selectedIndex ()= i
    }
    super.addPage(elem)(page)
  }

  def baseElem = <select size={size.toString}/>
  def properties = List(selectedIndex)
  def events = List(change)
}

/**
 * Provides several factories for creating Selects
 */
object Select {
  def selectedIndex(init: Option[Int] = None)(implicit observing: Observing): PropertyVar[Option[Int]] = PropertyVar("selectedIndex")(init)
  
  /**
   * @tparam T the type of the items
   * @param selected which item is initially selected?
   * @param items the fixed seq of items
   * @param renderer how to display items
   * @param size the height of the select, 1 for drop-downs
   * @param handleChange a function to call whenever the selection changes
   */
  def apply[T](selected: Option[T], items: Seq[T], renderer: T=>String, size: Int)(handleChange: Option[T]=>Unit)(implicit observing: Observing): Select[T] =
    apply(selected, SeqSignal(Val(items)), renderer, size)(handleChange)
  
  /**
   * @tparam T the type of the items
   * @param selected which item is initially selected?
   * @param items a SeqSignal representing the dynamic list of items
   * @param renderer how to display items
   * @param size the height of the select, 1 for drop-downs. Defaults to 1.
   * @param handleChange a function to call whenever the selection changes
   */
  def apply[T](selected: Option[T], items: SeqSignal[T], renderer: T=>String, size: Int = 1)(handleChange: Option[T]=>Unit)(implicit observing: Observing): Select[T] = {
    def _size = size
    new Select[T](items, renderer) {
      override val size = _size
      selectItem(selected)
      override protected def addPage(elem: Elem)(implicit page: Page) = {
        val ret = super.addPage(elem)(page)
        selectedItem.change foreach handleChange
        ret
      }
    }
  }
  /**
   * Creates a drop-down Select.
   * @tparam T the type of the items
   * @param items a SeqSignal representing the dynamic list of items
   * @param renderer how to display items
   */
  def apply[T](items: SeqSignal[T], renderer: T=>String)(implicit observing: Observing): Select[T] =
    new Select[T](items, renderer)
  /**
   * Creates a drop-down Select that uses the items' toString method to render them
   * @tparam T the type of the items
   * @param items a SeqSignal representing the dynamic list of items
   */
  def apply[T](items: SeqSignal[T])(implicit observing: Observing): Select[T] =
    new Select[T](items)
  /**
   * Creates a drop-down Select.
   * @tparam T the type of the items
   * @param items a Signal[Seq[T]] representing the dynamic list of items. When its value changes, a diff is calculated and used to update the select.
   * @param renderer how to display items
   */
  def apply[T](items: Signal[Seq[T]], renderer: T=>String)(implicit observing: Observing): Select[T] =
    new Select[T](SeqSignal(items), renderer)
  /**
   * Creates a drop-down Select that uses the items' toString method to render them
   * @tparam T the type of the items
   * @param items a Signal[Seq[T]] representing the dynamic list of items. When its value changes, a diff is calculated and used to update the select.
   */
  def apply[T](items: Signal[Seq[T]])(implicit observing: Observing): Select[T] =
    new Select[T](SeqSignal(items))
}
