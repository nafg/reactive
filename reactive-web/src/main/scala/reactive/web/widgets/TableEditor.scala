package reactive
package web
package widgets

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.util.{ A => _, _ }
import reactive._
import reactive.web._
import scala.util.Try
import scala.util.Failure

/**
 * Renders a table editor
 * Binds the following classes:
 *  new - click to add a new item
 *  save - click to persist changes
 *  items - contains one element which is dynamically repeated (using Repeater). That element can contain the following classes:
 *   o remove - click to remove the item
 *   o any name specified in cols - binds the Col/Row
 */
trait TableEditor[A] extends TableView[A] {

  // ABSTRACT
  type RowType <: EditableRow
  /**
   * Instantiate a blank item
   */
  def newItem: A
  /**
   * Apply all the actions to the data store
   */
  def save(actions: Seq[Edit]): Unit

  implicit def page: Page

  // ACTIONS
  sealed trait Edit {
    def item: A
    def inverse: Edit
  }
  case class Insert(item: A) extends Edit {
    def inverse = Delete(item)
  }
  case class Update[C](item: A, col: EditableCol[C], v: C) extends Edit {
    def inverse = Update(item, col, col.get(item))
  }
  case class Delete(item: A) extends Edit {
    def inverse = Insert(item)
  }

  def messages = Messages.defaultMessages

  /**
   * get and set form a lens to get and set the column value in a row.
   * editor turns that into a NodeSeq=>NodeSeq
   */
  class EditableCol[T](val selector: String, val get: A => T, val set: T => A => A, editor: T => Editor[T])(implicit observing: Observing) extends Col {
    private def rowMap[TT] = new AtomicRef(Vector.empty[(RowType, TT)])
    private def getOrElseUpdate[TT](map: AtomicRef[Vector[(RowType, TT)]])(row: RowType, makeNew: => TT): TT = map.run { xs =>
      xs.find(_._1 eq row) match {
        case Some((_, x)) => (xs, x)
        case None =>
          val x = makeNew
          (xs :+ (row, x), x)
      }
    }
    val validities = rowMap[Signal[Validity[T, NodeSeq]]]
    def validity(row: RowType): Signal[Validity[T, NodeSeq]] = getOrElseUpdate(validities)(row, renderer(row).value)

    val editors = rowMap[Editor[T]]
    val renderer = { row: RowType =>
      getOrElseUpdate(editors)(row, editor(get(row.item)))
    }

    override def render(row: RowType)(implicit page: Page) = {
      println("In render for (%s, %s)" format (row, this))

      def opt(i: Int) = Some(i) filter (-1 != _)
      selector #> (renderer(row) andThen
        PropertyVar.toggleClass("modified")(row.modifieds(cols.indexOf(this)).distinct) andThen
        onServer[KeyUp]{ // we have to go to the server because we don't know what rows will be adjacent at the moment the key is pressed until then.
          case KeyUp(38, _) => //Up
            opt(rows.now.indexOf(row)) filter (_ > 0) foreach { i =>
              val prevId = renderer(rows.now(i - 1)).id
              page.queue(js"document.getElementById($prevId).focus()")
            }
          case KeyUp(40, _) => //Down
            opt(rows.now.indexOf(row)) filter (_ < rows.now.size - 1) foreach { i =>
              val nextId = renderer(rows.now(i + 1)).id
              page.queue(js"document.getElementById($nextId).focus()")
            }
          case _ =>
        }
      )
    }
    override def toString = "EditableCol("+selector+")"
  }

  /**
   * Extends Row, adding a remove click handler
   */
  class EditableRow(val item: A) extends Row { this: RowType =>
    lazy val remove = DomEventSource.click
    lazy val removes = remove.eventStream map (_ => Delete(item))

    lazy val modifieds = cols.map {
      case c: EditableCol[_] =>
        val v = c.validity(this)
        val initial = v.now
        v.map(_ != initial)
      case _ => Val(false)
    }

    override def render = super.render &
      ".remove" #> remove
  }

  val newClicks = DomEventSource.click
  val inserts = newClicks.eventStream map (_ => Insert(newItem))

  val actions = Var[List[Edit]](Nil) =>> { as =>
    println(as.mkString("Actions: \n  ", "\n  ", ""))
  }
  val redoActions = Var[List[Edit]](Nil) =>> { rs =>
    println(rs.mkString("Redo actions: \n  ", "\n  ", ""))
  }

  override lazy val rows = SeqSignal(
    filterRows(initial).flatMap{ is =>
      def applyAction: Seq[RowType] => Edit => Seq[RowType] = xs => {
        case Insert(x) => xs :+ Row(x)
        case Delete(x) =>
          val (before, at) = xs span (_.item != x)
          before ++ at.drop(1)
        case _ => xs //TODO apply updates that don't come from UI
      }
      val current = actions.now.reverse.foldLeft(is)(applyAction(_)(_))
      SeqSignal(actions).deltas.map(d => SeqDelta.flatten(d :: Nil)).foldLeft(current){
        case (xs0, ds) =>
          ds.sortBy{
            case Remove(_, Insert(_)) => Int.MaxValue
            case _                    => 0
          }.foldLeft(xs0) {
            case (xs, Include(_, edit))              => applyAction(xs)(edit)
            case (xs, Remove(_, edit))               => applyAction(xs)(edit.inverse)
            case (xs, reactive.Update(_, old, edit)) => applyAction(applyAction(xs)(old.inverse))(edit)
          }
      }.hold(current)
    },
    { (a: Seq[RowType], b: Seq[RowType]) => LCS.lcsdiff[RowType, RowType](a, b, _.item == _.item) }
  )
  rows.deltas foreach { d => println("rows deltas: "+d) }
  val edits = (inserts | rows.flatMap {
    _.flatMap { r =>
      r.removes :: cols.collect {
        case c: EditableCol[_] => c.validity(r).distinct.change.collect {
          case Valid(v)        => Update(r.item, c, v)
          case Warning(v, msg) => Update(r.item, c, v)
        }
      }
    }.foldLeft(EventStream.empty[Edit])(_ | _)
  })
  edits foreach { e => println("Edit: "+e) }
  actions <<: (edits.map(Left(_)) | refreshes.map(Right(_))).foldLeft(actions.now){
    case (as, Left(e))  => e :: as
    case (_, Right(())) => Nil
  }
  redoActions <<: (edits | refreshes).map(_ => List.empty[Edit])

  override def fireRefresh {
    if (actions.now.isEmpty)
      super.fireRefresh
    else {
      lazy val msg: NodeSeq = <xml:group>
                                <p>You have usaved changes. Are you sure you want to discard them?</p>
                                <button onclick={
                                  onServer[Click]{ _ =>
                                    messages -= msg
                                    super.fireRefresh
                                  }.js
                                }>Yes</button>
                              </xml:group>
      messages += msg
    }
  }

  override def render = {
    super.render &
      ".unsaved" #> web.Cell {
        actions.map(_.isEmpty).distinct.map{
          case true  => ClearNodes
          case false => PassThru
        }
      } &
      ".new" #> newClicks &
      ".undo" #> {
        onServer[Click]{ _ =>
          actions.synchronized {
            if (actions.now.nonEmpty) {
              redoActions () = actions.now.head :: redoActions.now
              actions () = actions.now.tail
            }
          }
        } andThen PropertyVar("disabled").fromSignal(actions map (_.isEmpty))
      } &
      ".redo" #> {
        onServer[Click]{ _ =>
          actions.synchronized {
            if (redoActions.now.nonEmpty) {
              actions () = redoActions.now.head :: actions.now
              redoActions () = redoActions.now.tail
            }
          }
        } andThen PropertyVar("disabled").fromSignal(redoActions map (_.isEmpty))
      } &
      ".save" #> onServer[Click]{ _ => //TODO use nonblocking
        def doSave {
          Try {
            println("actions.now: "+actions.now)
            save(actions.now.reverse)
            refreshes fire ()
          } match {
            case Failure(e) =>
              messages += "Save failed: "+e.getMessage
            case _ =>
              messages += "Saved successfully!"
          }
        }
        val (warnings, errors) = cols.collect {
          case c: EditableCol[_] => c.validities.get.map(_._2.now)
        }.flatten.foldLeft((List[Warning[_, NodeSeq]](), List[Invalid[NodeSeq]]())){
          case ((ws, is), v: Valid[_])      => (ws, is)
          case ((ws, is), w: Warning[_, _]) => (w :: ws, is)
          case ((ws, is), i: Invalid[_])    => (ws, i :: is)
        }
        if (errors.nonEmpty)
          messages += "Some values are not valid. Please fix them first."
        else if (warnings.nonEmpty) {
          lazy val msg: NodeSeq =
            <xml:group>
              <p>
                Some values have warnings.
                Are you sure you want to save anyway?
              </p>
              <p><button class="btn btn-danger" onclick={ onServer[Click]{ _ => messages -= msg; doSave }.js }>Yes</button></p>
            </xml:group>
          messages += msg
        } else
          doSave
      }
  }
}

trait TableEditorPlus[A] extends TableEditor[A] {
  actions.distinct foreach { as => page queue js"window.isClean = ${as.isEmpty}" }

  override def render = {
    page queue js"""
      window.onbeforeunload = function(evt) {
        if(!window.isClean) {
          var reply = "You have unsaved changes!"
          if (!evt) evt = window.event
          if (evt) evt.returnValue = reply
          return reply
        }
      }
    """
    super.render
  }
}
