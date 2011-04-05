package reactive
package web
package html


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
) extends Repeater {
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
  val selectedIndex = Select.selectedIndex(Var(None: Option[Int])) updateOn change
  
  /**
   * A signal that represents the selected item as a T.
   */
  val selectedItem: Signal[Option[T]] = for {
    si <- selectedIndex.value
    is <- items
  } yield si.filter{i =>
    if(i < is.length) true else {println("WARNING: selectedIndex %d is out of bounds (%d)".format(i,is.length));false}
  } map is
  
  /**
   * Call this to select another (or no) item.
   */
  //TODO perhaps just use a Var?
  //TODO what about multiple selections? Use another class?
  def selectItem(item: Option[T]) {
    selectedIndex.value ()= item map {e=>items.now.indexOf(e)} filter(_ != -1)
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
  
  override protected def addPage(implicit page: Page) {
    super.addPage(page)
    items.change.foreach{is =>
      val i = selectedIndex.value.now map {_.min(is.length-1)}
      selectedIndex.value ()= i
    }
  }
  
  def baseElem = <select size={size.toString}/>
  def properties = List(selectedIndex)
  def events = List(change)
}

/**
 * Provides several factories for creating Selects
 */
object Select {
  def selectedIndex = DOMProperty("selectedIndex")
  
  /**
   * @tparam T the type of the items
   * @param selected which item is initially selected?
   * @param items the fixed seq of items
   * @param renderer how to display items
   * @param size the height of the select, 1 for drop-downs
   * @param handleChange a function to call whenever the selection changes
   */
  def apply[T](selected: Option[T], items: Seq[T], renderer: T=>String, size: Int)(handleChange: Option[T]=>Unit): Select[T] =
    apply(selected, SeqSignal(Val(items)), renderer, size)(handleChange)
  
  /**
   * @tparam T the type of the items
   * @param selected which item is initially selected?
   * @param items a SeqSignal representing the dynamic list of items
   * @param renderer how to display items
   * @param size the height of the select, 1 for drop-downs. Defaults to 1.
   * @param handleChange a function to call whenever the selection changes
   */
  def apply[T](selected: Option[T], items: SeqSignal[T], renderer: T=>String, size: Int = 1)(handleChange: Option[T]=>Unit): Select[T] = {
    def _size = size
    new Select[T](items, renderer) {
      override val size = _size
      selectItem(selected)
      override protected def addPage(implicit page: Page) {
        super.addPage(page)
        selectedItem.change foreach handleChange
      }
    }
  }
  /**
   * Creates a drop-down Select.
   * @tparam T the type of the items
   * @param items a SeqSignal representing the dynamic list of items
   * @param renderer how to display items
   */
  def apply[T](items: SeqSignal[T], renderer: T=>String): Select[T] =
    new Select[T](items, renderer)
  /**
   * Creates a drop-down Select that uses the items' toString method to render them
   * @tparam T the type of the items
   * @param items a SeqSignal representing the dynamic list of items
   */
  def apply[T](items: SeqSignal[T]): Select[T] =
    new Select[T](items)
  /**
   * Creates a drop-down Select.
   * @tparam T the type of the items
   * @param items a Signal[Seq[T]] representing the dynamic list of items. When its value changes, a diff is calculated and used to update the select.
   * @param renderer how to display items
   */
  def apply[T](items: Signal[Seq[T]], renderer: T=>String): Select[T] =
    new Select[T](SeqSignal(items), renderer)
  /**
   * Creates a drop-down Select that uses the items' toString method to render them
   * @tparam T the type of the items
   * @param items a Signal[Seq[T]] representing the dynamic list of items. When its value changes, a diff is calculated and used to update the select.
   */
  def apply[T](items: Signal[Seq[T]]): Select[T] =
    new Select[T](SeqSignal(items))
}
