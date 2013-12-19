package reactive
package web
package html

import scala.xml.Elem

import scala.annotation.tailrec

import reactive.logging.Logger

/**
 * Represents a select element in the DOM.
 * @tparam T the type of item rendered by this select
 * @param items a SeqSignal containing the items rendered by this select
 * @param renderer a function that determines the representation of items. Defaults to calling _.toString
 * @param size the height of the select. Defaults to 1, i.e., a drop-down.
 */
class Select[T](
  val items: SeqSignal[T],
  renderer: T => String = { t: T => t.toString },
  val size: Int = 1)(implicit observing: Observing, config: CanRenderDomMutationConfig) extends Repeater with Logger {

  case class UpdatedItemFromIndex(item: Option[T])
  case class UpdatedIndexFromItem(item: Option[Int])

  def renderer = config.domMutationRenderer

  /**
   * The change DOM event
   */
  val change = DomEventSource.change
  /**
   * The click DOM event
   */
  val click = DomEventSource.click

  val keyUp = DomEventSource.keyUp

  /**
   * The selectedIndex DOM property
   * Also when the select is rendered, this affects which option has the selected="selected" attribute.
   */
  val selectedIndex = Select.selectedIndex(Some(0) filter (_ <= items.now.length)) updateOn (change, keyUp)

  /**
   * A Var[Option[T]] that represents and sets the selected item.
   */
  val selectedItem: Var[Option[T]] = Var(None)

  private def adjustIndexFromDeltas(si: Int)(deltas: List[SeqDelta[_, _]]): Int = {
    deltas match {
      case Nil =>
        si
      case Include(i, _) :: rest if i <= si =>
        adjustIndexFromDeltas(si + 1)(rest)
      case Remove(i, _) :: rest if i < si =>
        adjustIndexFromDeltas(si - 1)(rest)
      case Remove(i, _) :: rest if i == si =>
        0
      case Batch(ms @ _*) :: rest =>
        adjustIndexFromDeltas(adjustIndexFromDeltas(si)(ms.toList))(rest)
      case other :: rest =>
        adjustIndexFromDeltas(si)(rest)
    }
  }

  /**
   * Call this to select another (or no) item.
   */
  //TODO what about multiple selections? Use another class?
  @deprecated("Use selectedItem ()= value instead", "0.2")
  def selectItem(item: Option[T]) {
    selectedIndex() = item.map(items.now.indexOf(_)).filter(_ != -1)
  }

  lazy val children = items.now.map { item: T =>
    RElem {
      if (selectedItem.now == Some(item))
        <option selected="selected">{ renderer(item) }</option>
      else
        <option>{ renderer(item) }</option>
    }
  }.signal

  items.change.foreach { is =>
    val i = selectedIndex.now map { _.min(is.length - 1) } filter { _ >= 0 }
    selectedIndex() = i
  }

  selectedIndex <<: items.deltas.map { d =>
    Some(adjustIndexFromDeltas(selectedIndex.now getOrElse -1)(d :: Nil)) filter (_ > -1)
  }

  selectedIndex <<: selectedItem.change.map { i =>
    i map items.now.indexOf[T] filter (_ > -1)
  }.nonrecursive =>> { x: Option[Int] => trace(UpdatedIndexFromItem(x)) }

  selectedItem <<: (selectedIndex.map { siOpt =>
    val is = items.now
    siOpt.filter(_ => is.nonEmpty).
      map(si => is(si min is.length - 1 max 0))
  }.nonrecursive =>> { x: Option[T] => trace(UpdatedItemFromIndex(x)) })

  def baseElem = <select size={ size.toString }/>
  def properties = List(selectedIndex)
  def events = List(change, click, keyUp)
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
  def apply[T](selected: Option[T], items: Seq[T], renderer: T => String, size: Int)(handleChange: Option[T] => Unit)(implicit observing: Observing, config: CanRenderDomMutationConfig): Select[T] =
    apply(selected, SeqSignal(Val(items)), renderer, size)(handleChange)(observing, config)

  /**
   * @tparam T the type of the items
   * @param selected which item is initially selected?
   * @param items a SeqSignal representing the dynamic list of items
   * @param renderer how to display items
   * @param size the height of the select, 1 for drop-downs. Defaults to 1.
   * @param handleChange a function to call whenever the selection changes
   */
  def apply[T](selected: Option[T], items: SeqSignal[T], renderer: T => String, size: Int = 1)(handleChange: Option[T] => Unit)(implicit observing: Observing, config: CanRenderDomMutationConfig): Select[T] = {
    def _size = size
    new Select[T](items, renderer)(observing, config) {
      override val size = _size
      selectedItem () = selected
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
  def apply[T](items: SeqSignal[T], renderer: T => String)(implicit observing: Observing, config: CanRenderDomMutationConfig): Select[T] =
    new Select[T](items, renderer)(observing, config)
  /**
   * Creates a drop-down Select that uses the items' toString method to render them
   * @tparam T the type of the items
   * @param items a SeqSignal representing the dynamic list of items
   */
  def apply[T](items: SeqSignal[T])(implicit observing: Observing, config: CanRenderDomMutationConfig): Select[T] =
    new Select[T](items)(observing, config)
  /**
   * Creates a drop-down Select.
   * @tparam T the type of the items
   * @param items a Signal[Seq[T]] representing the dynamic list of items. When its value changes, a diff is calculated and used to update the select.
   * @param renderer how to display items
   */
  def apply[T](items: Signal[Seq[T]], renderer: T => String)(implicit observing: Observing, config: CanRenderDomMutationConfig): Select[T] =
    new Select[T](SeqSignal(items), renderer)(observing, config)
  /**
   * Creates a drop-down Select that uses the items' toString method to render them
   * @tparam T the type of the items
   * @param items a Signal[Seq[T]] representing the dynamic list of items. When its value changes, a diff is calculated and used to update the select.
   */
  def apply[T](items: Signal[Seq[T]])(implicit observing: Observing, config: CanRenderDomMutationConfig): Select[T] =
    new Select[T](SeqSignal(items))(observing, config)
}
