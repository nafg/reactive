package reactive
package web

/**
 * Represents a select element in the DOM.
 * @tparam T the type of item rendered by this select
 * @param items a SeqSignal containing the items rendered by this select
 * @param renderer a function that determines the representation of items. Defaults to calling _.toString
 * @param size the height of the select. Defaults to 1, i.e., a drop-down.
 */
//TODO should renderer return a NodeSeq?
class Select[T](
  items: SeqSignal[T],
  renderer: T=>String = {t:T => t.toString},
  val size: Int = 1
) extends Repeater {
  /**
   * The change DOM event
   */
  val change = new JSEventSource[Change.type]
  /**
   * The selectedIndex DOM property
   * Also when the select is rendered, this affects which option has the selected="selected" attribute. 
   */
  val selectedIndex = new JSProperty[Option[Int]] {
    val value = Var[Option[Int]](None)
    def fromString(s: String) = net.liftweb.util.Helpers.asInt(s).toOption.filter(_ != -1)
    def asString(v: Option[Int]) = v.map(_.toString) getOrElse "-1"
    def name = "selectedIndex"
    def elemId = id
    this updateOn change
  }
  /**
   * A signal that represents the selected item as a T.
   */
  val selectedItem: Signal[Option[T]] = selectedIndex.value map {optN => optN map {n=>items.now(n)}}
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
  
  def baseElem = <select size={size.toString}/>
  def properties = List(selectedIndex)
  def events = List(change)
}

/**
 * Provides several factories for creating Selects
 */
object Select {
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
