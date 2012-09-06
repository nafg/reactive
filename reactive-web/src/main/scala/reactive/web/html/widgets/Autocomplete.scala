package reactive.web.html.widgets

import scala.xml._
import reactive._
import web._
import html._
import javascript._
import JsTypes._
import net.liftweb.common.Loggable
import net.liftweb.common.Full
import net.liftweb.http._
import js.JsCmd
import js.JsCmds.{ RedirectTo, Noop }
import net.liftweb.util._
import Helpers._
import scala.annotation.tailrec

class AutocompleteCss {
  def autocomplete = "reactive-autocomplete"
  def empty = "reactive-empty"
  def input = "reactive-autocomplete-input"
  def candidatesCont = "reactive-autocomplete-candidates"
  def candidate = "reactive-autocomplete-candidate"
  def selected = "reactive-autocomplete-selected"
  def selectedColor = "yellow"

  def style = <head><style type="text/css">{
    "." + autocomplete + " ." + empty + " {display: none;}\n" +
      "." + autocomplete + " {position: relative; width: 30%;}\n" +
      "." + input + " {width: 100%; margin-bottom: 0 !important;}\n" +
      "." + selected + " {background-color: " + selectedColor + ";}\n" +
      "." + candidatesCont + """ {
      width: 99%;
      border-left:1px solid #BBB;
      border-right: 1px solid #BBB;
      border-bottom: 1px solid #BBB;
      padding: 0.1em;
    }"""
  }</style></head>
}

class Autocomplete[T](
  updateItems: String => Seq[T],
  renderItem: T => String = { t: T => t.toString })(implicit observing: Observing,
    config: CanRenderDomMutationConfig) extends RElem with Logger {

  // public values

  def renderer = config.domMutationRenderer

  lazy val value = selectedItem

  //TODO should this be a Config?
  def css = new AutocompleteCss

  // protected values

  protected lazy val items = BufferSignal[T]() <<: input.value.map(updateItems)

  object input extends TextInput {
    override def baseElem =
      <input type="text" autocomplete="off" class={ css.input }/>
    value updateOn keyUp
    keyUp.eventStream ?>> {
      case KeyUp(downArrow, _) if repeat.children.now.size > 0 => Javascript {
        repeat.children.now.head.focus()
      }
    }
  }

  protected val selectedItem: Var[Option[T]] = Var(None)

  object repeat extends Repeater {
    def renderer = config.domMutationRenderer
    def baseElem = <div/>
    val className = PropertyVar.className.fromSignal{
      items map { xs => css.candidatesCont + (if (xs.isEmpty) " " + css.empty else "") }
    }
    val properties = List(className)
    def events = Nil
    lazy val children = SeqSignal[RElem](items.now.map(Candidate(_)).signal)
  }

  // private values

  private val selectedCandidate = Var(Option.empty[Candidate])

  private val (downArrow, upArrow, tabKey, enterKey, rightArrow) = (40, 38, 9, 13, 39)

  private case class Candidate(item: T) extends RElem {
    val focus = new DomEventSource[Focus]
    val blur = new DomEventSource[Blur]
    val keyup = DomEventSource.keyUp

    lazy val focusState = (focus.eventStream map (_ => true)) | (blur.eventStream map (_ => false)) hold false
    val className = PropertyVar.className.fromSignal(focusState map { b => css.candidate + (if (b) " " + css.selected else "") })

    override def baseElem = <div tabindex="0">{ renderItem(item) }</div>
    override val events = List(focus, blur, keyup)
    override val properties = List(className)
    lazy val rchildren = repeat.children.now
    lazy val idx = rchildren.indexWhere(_.id == this.id)

    selectedItem <<: focusState.map(b => Some(item).filter(_ => b))

    keyup.eventStream ?>> {
      case KeyUp(`upArrow`, _) =>
        Javascript {
          (rchildren lift (idx - 1) getOrElse input).focus()
        }
      case KeyUp(`downArrow`, _) =>
        Javascript {
          (rchildren lift (idx + 1) getOrElse rchildren.head).focus()
        }
      case KeyUp(`tabKey` | `enterKey` | `rightArrow`, _) =>
        input.value() = selectedItem.now map renderItem getOrElse ""
        Javascript {
          input.focus()
        }
    }
  }

  override def renderer(implicit p: Page) =
    e => super.renderer(p)(e).copy(child = css.style :+ input.render :+ repeat.render)

  def baseElem = <div class={ css.autocomplete }/>
  def properties = Nil
  def events = Nil
}

object Autocomplete {
  /**
   * Creates an Autocomplete
   * @tparam T the type of the items
   * @param updateItems a function that deals with the String
   * entered in the autocompleted input to update the list of items
   * @param renderItem how to display items
   */
  def apply[T](
    updateItems: String => Seq[T],
    renderItem: T => String)(
      implicit observing: Observing,
      config: CanRenderDomMutationConfig): Autocomplete[T] =
    new Autocomplete[T](updateItems, renderItem)(observing, config)

  /**
   * Creates an Autocomplete that uses the items' toString method to render them
   * @tparam T the type of the items
   * @param updateItems a function that deals with the String
   * entered in the autocompleted input to update the list of items
   */
  def apply[T](
    updateItems: String => Seq[T])(
      implicit observing: Observing,
      config: CanRenderDomMutationConfig): Autocomplete[T] =
    new Autocomplete[T](updateItems)(observing, config)

}

