package reactive
package web
package widgets

import scala.xml.NodeSeq

import net.liftweb.common.Failure
import net.liftweb.util.Helpers._
import net.liftweb.util._

import reactive._
import reactive.web._
import reactive.web.javascript._

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

  val messages = Messages.is

  /**
   * get and set form a lens to get and set the column value in a row.
   * editor turns that into a NodeSeq=>NodeSeq
   */
  class EditableCol[T](val selector: String, val get: A => T, val set: T => A => A, editor: T => Editor[T])(implicit observing: Observing) extends Col {
    import scala.collection.mutable.Map
    val validities = Map.empty[RowType, Signal[Validity[T, NodeSeq]]]
    def validity(row: RowType): Signal[Validity[T, NodeSeq]] = validities getOrElseUpdate (row, renderer(row).value)

    val editors = Map.empty[A, Editor[T]]
    val renderer = { row: RowType =>
      editors.getOrElseUpdate(row.item, editor(get(row.item)))
    }

    override def render(row: RowType)(implicit page: Page) = {
      println("In render for (%s, %s)" format (row, this))

      def opt(i: Int) = Some(i) filter (-1 !=)
      selector #> (renderer(row) andThen
        PropertyVar.appendClass{ row.modifieds(cols.indexOf(this)).distinct.map(m => Some("modified") filter (_ => m)) } andThen
        onServer[KeyUp]{ // we have to go to the server because we don't know what rows will be adjacent at the moment the key is pressed until then. 
          case KeyUp(38, _) => //Up
            opt(rows.now.indexOf(row)) filter (_ > 0) foreach { i =>
              val prevId = renderer(rows.now(i - 1)).id
              Javascript {
                window.document.getElementById(prevId).focus()
              }
            }
          case KeyUp(40, _) => //Down
            opt(rows.now.indexOf(row)) filter (_ < rows.now.size - 1) foreach { i =>
              val nextId = renderer(rows.now(i + 1)).id
              Javascript {
                window.document.getElementById(nextId).focus()
              }
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
      case c: EditableCol[Any] =>
        actions map (_ exists {
          case Update(i, `c`, v) if i == item && v != c.get(item) => true
          case _ => false
        })
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
      val current = actions.now.foldLeft(is)(applyAction(_)(_))
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
    case (as, Left(e)) => e :: as
    case _             => Nil
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
          tryo {
            println("actions.now: "+actions.now)
            save(actions.now)
            refreshes fire ()
          } match {
            case Failure(msg, _, _) =>
              Reactions.inAnyScope(page) {
                Page.withPage(page) {  // TODO is this necessary?
                  messages += "Save failed: "+msg
                }
              }
            case _ =>
              Reactions.inAnyScope(page) {
                Page.withPage(page) {  // TODO is this necessary?
                  messages += "Saved successfully!"
                }
              }
          }
        }
        val (warnings, errors) = cols.collect {
          case c: EditableCol[_] => c.validities.values.map(_.now)
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
  def isClean = window.get("isClean").asInstanceOf[Assignable[JsTypes.JsBoolean]]
  actions.distinct foreach { as => Javascript { isClean := as.isEmpty } }

  override def render = {
    Javascript {
      window.onbeforeunload := { e: JsExp[JsTypes.JsObj] =>
        If(!isClean) {
          val reply = JsVar[JsTypes.JsString]()
          reply := "You have unsaved changes!"
          val evt = JsVar[JsTypes.JsObj]()
          evt := e
          If (evt === null) { evt := window.event }
          If (evt !== null) { evt.get("returnValue") := reply }
          Return(reply)
        }
      }
    }
    super.render
  }
}
