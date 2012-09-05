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

  protected val style = <style type="text/css">{
    "." + css.autocomplete + " ." + css.empty + " {display: none;}\n" +
      "." + css.autocomplete + " {position: relative; width: 30%;}\n" +
      "." + css.input + " {width: 100%; margin-bottom: 0 !important;}\n" +
      "." + css.selected + " {background-color: " + selectedColor + ";}\n" +
      "." + css.candidatesCont + """ {
      width: 99%;
      border-left:1px solid #BBB;
      border-right: 1px solid #BBB;
      border-bottom: 1px solid #BBB;
      padding: 0.1em;
    }"""
  }</style>
}

class Autocomplete[T](
  updateItems: String => Seq[T],
  renderItem: T => String = { t: T => t.toString })(implicit observing: Observing,
    config: CanRenderDomMutationConfig) extends RElem with Logger {

  // public values

  def renderer = config.domMutationRenderer

  lazy val value = selectedItem

  def css = new AutocompleteCss

  // protected values

  protected lazy val items = BufferSignal[T]()

  val input = new TextInput {
    override def baseElem =
      <input type="text" autocomplete="off" class={ css.input }/>
  }
  input.value.updateOn(input.keyUp)
  input.value.change foreach { str => items() = updateItems(str) }
  input.keyUp.foreach { (x: $[JsObj]) =>
    Return(x)
  }.$
  input.keyUp.eventStream ?>> {
    case KeyUp(40, _) if repeat.children.now.size > 0 => Javascript {
      repeat.children.now.head.focus()
    }
  }

  protected val selectedItem: Var[Option[T]] = Var(None)

  val repeat: Repeater = new Repeater {
    def renderer = config.domMutationRenderer
    def baseElem = <div/>
    val cssClass = PropertyVar("className", "class")(
      css.candidatesCont + " " + css.empty)
    items.change ->> {
      cssClass() =
        if (items.now.size > 0) (cssClass.now split " ")
          .filterNot(_ == css.empty) mkString " "
        else css.candidatesCont + " " + css.empty
    }
    def properties = List(cssClass)
    def events = Nil
    lazy val children = SeqSignal(items map {
      _ map (item => Candidate(item): RElem)
    })
    children.change ->> { alert(children.now mkString (" || ")) }
  }

  // private values

  private val selectedCandidate: Var[Option[Candidate[T]]] = Var(None)

  private case class Candidate[U <: T](item: U) extends RElem {
    val focus = new DomEventSource[Focus]
    val blur = new DomEventSource[Blur]
    val keyup = DomEventSource.keyUp
    val className =
      PropertyVar("className", "class")(css.candidate)
    override def baseElem =
      <div tabindex="0">{ renderItem(item) }</div>
    override def events = List(focus, blur, keyup)
    override def properties = List(className)
    lazy val rchildren = repeat.children.now
    lazy val idx = rchildren.indexWhere(_.id == this.id)
    focus.eventStream ->> {
      className() = css.selected + " " + className.now
      selectedItem() = Some(item)
    }
    blur.eventStream ->> {
      className() = (className.now split " ")
        .filterNot(_ == css.selected) mkString " "
      selectedItem() = None
    }
    keyup ?>> {
      case KeyUp(38, _) =>
        Javascript {
          (if (rchildren isDefinedAt (idx - 1)) rchildren(idx - 1)
          else input).focus()
        }
      case KeyUp(40, _) =>
        Javascript {
          (if (rchildren isDefinedAt (idx + 1)) rchildren(idx + 1)
          else rchildren.head).focus()
        }
      case KeyUp(code, _) if (List(9, 13, 39) contains code) =>
        val selOpt = selectedItem.now filterNot (_ => selectedItem.now.isEmpty)
        input.value() = renderItem(selOpt.get)
        Javascript { input.focus() }
    }
  }

  override def renderer(implicit p: Page) =
    e => super.renderer(p)(e).copy(child = css.style :+ input.render :+ repeat.render)

  def baseElem = <div class={ css.autocomplete }/>
  def properties = Nil
  def events = Nil
}

object AutoComplete {
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
      config: CanRenderDomMutationConfig): AutoComplete[T] =
    new AutoComplete[T](updateItems, renderItem)(observing, config)

  /**
   * Creates an Autocomplete that uses the items' toString method to render them
   * @tparam T the type of the items
   * @param updateItems a function that deals with the String
   * entered in the autocompleted input to update the list of items
   */
  def apply[T](
    updateItems: String => Seq[T])(
      implicit observing: Observing,
      config: CanRenderDomMutationConfig): AutoComplete[T] =
    new AutoComplete[T](updateItems)(observing, config)

}

