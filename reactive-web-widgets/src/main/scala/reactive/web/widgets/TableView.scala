package reactive
package web
package widgets

import scala.xml.NodeSeq

import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

/**
 * Extensible trait to display tabular data
 */
trait TableView[A] {
  /**
   * The type of modifiers to `fetch`. For instance,
   * if you use `lift-mapper` this may be `Seq[QueryParam[MyModelType]]`.
   * If you use ScalaQuery then this may be `Query[MyModelType]=>Query[MyModelType`.
   */
  type FetchParam

  /**
   * The items will be refreshed when the element
   * specified by this selector is clicked.
   */
  def refreshSelector = ".refresh"
  /**
   * The items will be rendered in `Repeater`
   * in the element specified by this selector.
   */
  def repeaterSelector = ".items"

  implicit def canRenderDomMutationConfig: CanRenderDomMutationConfig

  /**
   * A column -- binds one value of a row
   */
  trait Col {
    /**
     * The css selector to render this column inside of
     */
    def selector: String
    /**
     * Render the actual value for a given row
     */
    val renderer: RowType => NodeSeq => NodeSeq
    /**
     * The css selector expression that renders this column.
     */
    def render(row: RowType)(implicit page: Page): CssSel = selector #> renderer(row)

    override def toString = "Col{" + selector + "}"
  }

  /**
   * Creates an column with the given selector and renderer function
   */
  def Col(selector0: String, renderer0: RowType => NodeSeq => NodeSeq): Col =
    new Col {
      val selector = selector0
      val renderer = renderer0
    }

  /**
   * A row -- wraps an item and can render it.
   * You may want to subclass `Row` and set `RowType` to your subclass.
   */
  trait Row { this: RowType =>
    /**
     * The item currently in this row (for immutable items, this may
     * point to different items over time).
     */
    def item: A
    def render: CssSel = cols map (_.render(this)) reduceLeft (_ & _)
  }

  /**
   * The actual subtype of Row used
   */
  type RowType <: Row

  implicit def page: Page

  implicit def observing: Observing

  /**
   * The columns
   */
  def cols: List[Col]

  protected lazy val params: Signal[Seq[FetchParam]] = Val(Nil)

  /**
   * Get the items from the data source
   * @param params Data that parameterizes the fetch, such as query parameters.
   */
  def fetch(params: Seq[FetchParam]): Seq[RowType]

  /**
   * Events from this event stream cause `initial` to be reset to `fetch`.
   * Thus if the data in the data source changed,
   * firing an event from `refreshes` will
   * cause the new data to be loaded.
   */
  val refreshes = new EventSource[Unit]

  protected lazy val initialImpl = for {
    _ <- refreshes hold ()
    filters <- params
  } yield fetch(filters)

  /**
   * The set of `Row`s as they are in the data source.
   * In-memory modifications, such as filters or edits, may be
   * applied to `initial` to yield `rows`. They will be reapplied
   * when `initial` changes.
   *
   * If you have a data source that notifies of changes (e.g., CouchDB),
   * you may want to override initial to take those notifications into
   * account.
   */
  def initial: Signal[Seq[RowType]] = initialImpl

  /**
   * Fire a unit event from `refreshes`.
   * Override to prevent refreshing in certain circumstances
   * (for instance `TableEditor` prompts if there are
   * unsaved changes).
   */
  protected def fireRefresh(): Unit = refreshes fire ()

  /**
   * Wrap an item in a `Row`
   */
  def Row(item: A): RowType

  /**
   * Modify the rows displayed relative to those loaded (`initial`).
   * By default the `identity` function (no change).
   */
  def filterRows: Signal[Seq[RowType]] => Signal[Seq[RowType]] = identity

  private lazy val rowsImpl = SeqSignal(filterRows(initial))
  /**
   * The rows to display
   */
  def rows: SeqSignal[RowType] = rowsImpl

  def render =
    refreshSelector #> onServer[Click]{ _ =>
      fireRefresh()
    } &
      repeaterSelector #> Repeater {
        rows.now.map(_.render).signal
      }
}
