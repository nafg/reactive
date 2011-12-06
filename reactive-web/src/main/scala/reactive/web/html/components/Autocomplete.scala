package reactive
package web
package html
package components

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

class Autocomplete[T](
  updateItems: String => Seq[T],
  renderItem: T => String = { t: T => t.toString })(implicit observing: Observing,
    config: CanRenderDomMutationConfig) extends RElem with Logger {

  // public values

  def renderer = config.domMutationRenderer

  lazy val value = selectedItem

  var autocompleteCssClass = "reactive-autocomplete"
  var emptyCssClass = "reactive-empty"
  var inputCssClass = "reactive-autocomplete-input"
  var candidatesContCssClass = "reactive-autocomplete-candidates"
  var candidateCssClass = "reactive-autocomplete-candidate"
  var selectedCssClass = "reactive-autocomplete-selected"
  var selectedColor = "yellow"

  // protected values

  protected val style = <style type="text/css">{
    "." + autocompleteCssClass + " ." + emptyCssClass + " {display: none;}\n" +
    "." + autocompleteCssClass + " {position: relative; width: 30%;}\n" +
      "." + inputCssClass + " {width: 100%; margin-bottom: 0 !important;}\n" +
      "." + selectedCssClass + " {background-color: " + selectedColor + ";}\n" +
      "." + candidatesContCssClass + """ {
      width: 99%;
      border-left:1px solid #BBB;
      border-right: 1px solid #BBB;
      border-bottom: 1px solid #BBB;
      padding: 0.1em;
    }"""
  }</style>

  protected lazy val items = BufferSignal[T]()

  protected val input = new TextInput {
    override def baseElem =
      <input type="text" autocomplete="off" class={ inputCssClass }/>
  }
  input.value.updateOn(input.keyUp)
  input.value.change foreach { str => items() = updateItems(str) }
  chooseCandidate(input.keyUp, true)

  protected val selectedItem: Var[Option[T]] = Var(None)

  protected val repeat: Repeater = new Repeater {
    def renderer = config.domMutationRenderer
    def baseElem = <div />
    val cssClass = PropertyVar("className", "class")(
      candidatesContCssClass + " " + emptyCssClass)
    items.change ->> {
      if (items.now.size > 0) cssClass() = (cssClass.now split " ")
        .filterNot(_ == emptyCssClass) mkString " "
      else cssClass() = candidatesContCssClass + " " + emptyCssClass
    }
    def properties = List(cssClass)
    def events = Nil
    val children = items map {
      _ map (item => candidate(item))
    }
  }

  protected def chooseCandidate(evSrc: DomEventSource[KeyUp], inInput: Boolean) {
    evSrc ?>> {
      case KeyUp(38, _) if !inInput => evSrc.jsEventStream fire {
        JsRaw("""function(){
      selected = $('.""" + selectedCssClass + """');
      prev = selected.prev();
      if (prev.size() == 0)
        $('.""" + inputCssClass + """').focus();
      else
        prev.focus();
    }()""")
      }
      case KeyUp(40, _) => evSrc.jsEventStream fire {
        JsRaw("""function(){
      selected = $('.""" + selectedCssClass + """');
      next = selected.next();
      if (next.size() == 0)
        $('.""" + candidateCssClass + """:first').focus();
      else
        next.focus();
    }()""")
      }
      case KeyUp(code, _) if (!inInput &&
        List(9, 13, 39).exists(_ == code)) =>
        val selOpt = selectedItem.now filterNot (_ => selectedItem.now.isEmpty)
        input.value() = renderItem(selOpt.get)
        evSrc.jsEventStream fire {
          JsRaw("""function(){
            $('.""" + inputCssClass + """').focus();
          }()""")
        }
    }
  }

  // private values

  private def candidate(item: T): RElem = new RElem {
    val focus = new DomEventSource[Focus]
    val blur = new DomEventSource[Blur]
    val keyup = DomEventSource.keyUp
    val className =
      PropertyVar("className", "class")(candidateCssClass)
    override def baseElem =
      <div tabindex="0">{ renderItem(item) }</div>
    override def events = List(focus, blur, keyup)
    override def properties = List(className)
    chooseCandidate(keyup, false)
    focus ->> {
      if (!className.now.contains(selectedCssClass)) {
        className() = selectedCssClass + " " + className.now
        selectedItem() = Some(item)
      }
    }
    blur ->> {
      if (items.now.size > 0) {
        className() = (className.now split " ")
          .filterNot(_ == selectedCssClass) mkString " "
        selectedItem() = None
      }
    }
  }

  // redefined values

  override def renderer(implicit p: Page) =
    e => super.renderer(p)(e).copy(child = style :+ input.render :+ repeat.render)

  def baseElem = <div class={ autocompleteCssClass }/>
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
