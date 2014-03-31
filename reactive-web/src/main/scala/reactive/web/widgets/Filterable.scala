package reactive
package web
package widgets

import net.liftweb.util.{ CssSel, PassThru }
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

/**
 * Defines a type of filter.
 * Examples include selecting from
 * a choice of values, pagination, and sort headers.
 * @tparam P the type of the criteria
 */
trait Filter[P] {
  /**
   * The filter criteria
   */
  val value: Signal[P]
}

/**
 * A filter that mixes this in can be rendered
 */
trait FilterUI {
  implicit def page: Page
  /**
   * Render the UI for the filter,
   * to allow users to interact with the filter's value
   */
  def render: CssSel
}

/**
 * A `Filter` that limits items to one pageful,
 * allowing for a pagination widget.
 * @param rowsPerPage The number of rows on a page
 * @param rows        A function-valued signal, where the function returns the total number of rows.
 *                    You can have the signal depend on other filters that affect the number of rows.
 */
class Paginator(val rowsPerPage: Int, rows: Signal[() => Int])(implicit observing: Observing, val page: Page) extends Filter[Int] with FilterUI {
  /**
   * The page number
   */
  val value = Var[Int](0)
  /**
   * The first row to display (the page number multiplied by `rowsPerPage`)
   */
  val first = value map (_ * rowsPerPage)

  /**
   * Which pages to display in the pagination widget.
   * The default implementation displays the previous
   * 10 pages and the next 10 pages, and 20, 100, and 1000 pages in
   * either direction (all clipped to the valid range).
   */
  def pagesToDisplay(curPage: Int, numPages: Int) = {
    List(curPage - 1020, curPage - 120, curPage - 20) ++
      (curPage - 10 to curPage + 10) ++
      List(curPage + 20, curPage + 120, curPage + 1020) filter {
        n => n >= 0 && n < numPages
      }
  }

  /**
   * The selector for the element containing the pagination widget.
   * This element should contain an element for each kind of
   * navigation (first, previous, pages, etc.), each of which
   * may contain the element that will actually be made clickable.
   * (Presumably you can override `linkSelector` to `"*"` to
   * make the navigation element itself clickable).
   * Example (for twitter bootstrap): {{{
   * <div class=".pagination">
   *   <ul>
   *     <li class=".prev"><a>&lt;</a></li>
   *     <li class=".pages"><a></a><li>
   *     <li class=".next"><a>&gt;</a><li>
   *   </ul>
   * </div>
   * }}}
   */
  def selector = ".pagination"
  def firstSelector = ".first"
  def prevSelector = ".prev"
  def nextSelector = ".next"
  def lastSelector = ".last"
  def linkSelector = "a"
  /**
   * The element with this selector will
   * be repeated for each page number you can click.
   * A page number will be rendered inside its `linkSelector`.
   */
  def pagesSelector = ".pages"
  /**
   * This class will be added to elements that should not be clicked,
   * such as `firstSelector` when you are on the first page.
   */
  def disabledClass = "disabled"
  /**
   * This class will be added to the `pagesSelector` of the current page.
   */
  def activeClass = "active"

  def render = selector #> reactive.web.Cell {
    for {
      pages <- rows.map { r =>
        val rs = r()
        rs / rowsPerPage + (rs % rowsPerPage min 1)
      }
      page0 <- value
      curPage = page0 min pages - 1
    } yield {
      val is = pagesToDisplay(curPage, pages)

      (firstSelector+" [class+]") #> (if (curPage > 0) "" else disabledClass) andThen
        (firstSelector+" *") #> (linkSelector #> onServer { _: Click => value() = 0 }) andThen
        (prevSelector+" [class+]") #> (if (curPage > 0) "" else disabledClass) andThen
        (prevSelector+" *") #> (linkSelector #> onServer { _: Click => value() = curPage - 1 max 0 }) andThen
        pagesSelector #> is.map { i =>
          (pagesSelector+" [class+]") #> (if (i == curPage) activeClass else "") andThen
            (pagesSelector+" *") #> (
              linkSelector #> onServer { _: Click => value() = i } andThen
              (linkSelector+" *") #> (i + 1)
            )
        } andThen
        (nextSelector+" [class+]") #> (if (curPage < pages - 1) "" else disabledClass) andThen
        (nextSelector+" *") #> (linkSelector #> onServer { _: Click => value() = curPage + 1 min pages - 1 max 0 }) andThen
        (lastSelector+" [class+]") #> (if (curPage < pages - 1) "" else disabledClass) andThen
        (lastSelector+" *") #> (linkSelector #> onServer { _: Click => value() = pages - 1 max 0 })
    }
  }
}

/*
 * TODO text filter and choice filter.
 * The latter, and often the former, are applied to a single column, which means
 * they need to (a) be able to get the column's value for the case of post filter,
 * and (b) know the column for database query purposes.
 * Text filters can be for more than one column, so as above but in plural.
 */

/**
 * Use with `PreFilterable`. Mix in to regular `Filter`s and define `param`.
 */
trait PreFilter[FetchParam] { this: Filter[_] =>
  /**
   * A `Signal` (which should depend on `value`)
   * that specifies the `FetchParam`.
   */
  def param: Signal[FetchParam]
}

/**
 * A Filter that is applied to the rows in-memory,
 * after they have already been retrieved from the
 * data source.
 */

trait PostFilter[RowType] { this: Filter[_] =>
  /**
   * Filter the rows
   */
  def filter: Signal[Seq[RowType] => Seq[RowType]]
}

/**
 * A subclass of `Paginator` that works in memory
 */
class PostPaginator[RowType](rowsPerPage: Int, rows: Signal[() => Int])(implicit observing: Observing, page: Page) extends Paginator(rowsPerPage, rows) with PostFilter[RowType] {
  lazy val filter = first map { f => { (_: Seq[RowType]) drop f take rowsPerPage } }
}

/**
 * Base mixin for TableView/TableEditor that adds filters -- the ability to manage
 * signals that parameterize the data store fetch, or that perform some
 * post processing on the rows.
 * Do not mix in `Filterable` directly (it's sealed). Instead use `PreFilterable` and/or `PostFilterable`.
 */
sealed trait Filterable[A] extends TableView[A] {
  /**
   * The column to sort by and the sort direction, plus an optional `Ordering` for
   * in-memory sorting
   */
  case class SortState(col: Col, ascending: Boolean, ordering: Option[Ordering[RowType]] = None) {
    def reverse = SortState(col, !ascending, ordering map (_.reverse))
  }
  /**
   * A `Filter` that lets you click column headers to sort the rows by that column.
   * Clicking the column that the rows are already sorted by will reverse the sort direction.
   */
  class SortHeaders(initial: Option[SortState] = None)(implicit val page: Page) extends Filter[Option[SortState]] with FilterUI {
    val value = Var(initial)

    protected def sortStateForCol(c: Col) = SortState(c, true)

    def renderCol(col: Col)(renderer: NodeSeq => NodeSeq): CssSel = {
      col.selector #> (".sort-col" #> renderer)
    }
    //TODO render current sort state cue
    def render =
      cols map { c =>
        renderCol(c)(onServer[Click]{ _ =>
          value () = value.now match {
            case Some(s) => Some(s.reverse)
            case _       => Some(sortStateForCol(c))
          }
        })
      } reduceLeft (_ & _)
  }
}

/**
 * This mixin supports filters that are supply modifiers to `fetch`.
 */
trait PreFilterable[A] extends Filterable[A] {
  type PreFilter = widgets.PreFilter[FetchParam]

  /**
   * Filters that are applied by the database etc.
   * Changes to preFilters, or values of its filters, trigger calls to ''fetch''
   * and all items are reloaded (for `TableEditor`, modifications are re-applied
   * to the new items).
   */
  val preFilters: SeqSignal[Filter[_] with PreFilter]

  override protected lazy val params: Signal[Seq[FetchParam]] = preFilters.now.map(_.param).signal.sequence

  override def render = super.render &
    preFilters.now.collect{ case ui: FilterUI => ui }.foldLeft("thisbetternotexist" #> PassThru)(_ & _.render)
}

/**
 * This mixin supports filters that are applied in memory
 */
trait PostFilterable[A] extends Filterable[A] {
  type PostFilter = widgets.PostFilter[RowType]

  /**
   * Filters that are applied in-memory.
   * They are applied automatically.
   */
  val postFilters: SeqSignal[PostFilter]

  protected lazy val mergedPostFilters: Signal[Seq[Seq[RowType] => Seq[RowType]]] = postFilters.now.map(_.filter).signal.sequence

  override def filterRows: Signal[Seq[RowType]] => Signal[Seq[RowType]] = super.filterRows andThen {
    for {
      rs <- _
      filters <- mergedPostFilters
    } yield filters.foldLeft(rs) {
      case (rs, filter) => filter(rs)
    }
  }

  override def render = super.render &
    postFilters.now.collect{ case ui: FilterUI => ui }.foldLeft("thisbetternotexist" #> PassThru)(_ & _.render)

  /////////////////////
  // SORT BY COLUMN  //
  /////////////////////

  /**
   * A column with an `Ordering` that specifies how to sort the rows
   */
  trait OrderedCol extends Col {
    def rowOrdering: Ordering[RowType]
  }

  /**
   * Creates a sortable column (`OrderedCol`).
   * @param selector0 The css selector inside of which to render the column
   * @param rendered  The rendering function
   * @param get       Gets the value of the item on which the ordering is based
   */
  def sortCol[T](selector0: String, renderer0: RowType => NodeSeq => NodeSeq, get: A => T)(implicit ordering: Ordering[T] = null) =
    new OrderedCol {
      val selector = selector0
      val renderer = renderer0
      def rowOrdering: Ordering[RowType] = ordering on { r => get(r.item) }
    }

  /**
   * A subclass of `SortHeaders` that sorts the rows in memory. Normally only works for `OrderedCol`s
   */
  class PostSortHeaders(initial: Option[SortState] = None)(implicit page: Page) extends SortHeaders(initial) with PostFilter {
    override def sortStateForCol(c: Col) = c match {
      case c: OrderedCol => SortState(c, true, Option(c.rowOrdering))
      case c             => SortState(c, true, None)
    }

    lazy val filter = value.map { sort =>
      { rs: Seq[RowType] =>
        sort match {
          case Some(s: SortState) =>
            s.ordering match {
              case Some(o) => rs.sorted(o)
              case _       => rs
            }
          case _ =>
            rs
        }
      }
    }
  }
}
