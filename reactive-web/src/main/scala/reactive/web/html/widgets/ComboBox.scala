package reactive
package web
package html
package widgets

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

// class ComboBox[T](
//   allItems: Seq[T],
//   renderItem: T => String = { t: T => t.toString })(implicit observing: Observing,
//     config: CanRenderDomMutationConfig) extends Autocomplete[T](
//   (_: String) => allItems, renderItem)(observing, config) {

//   override def chooseCandidate(evSrc: DomEventSource[KeyUp], inInput: Boolean) {}

//   input.keyUp.eventStream.distinct.nonrecursive ?>> {
//     case KeyUp(38, _) => repeat.selectedIndex() = Some(
//     (repeat.selectedIndex.now getOrElse 0) - 1)
//     case KeyUp(40, _) => repeat.selectedIndex() = Some(
//       (repeat.selectedIndex.now getOrElse 0) + 1)
//   }

//   input.value.change.distinct.nonrecursive foreach { str =>
//     repeat.selectedItem() = 
//       allItems filter (c => renderItem(c).startsWith(str)) headOption
//   }

//   override val repeat = new Select[T](SeqSignal(Val(allItems)),
//     renderItem)(observing, config) {
//     override val size = allItems.size
//     val propSize: PropertyVar[Option[Int]] =
//       PropertyVar("size")(Some(size))
//     override def properties = List(selectedIndex, propSize)
//   }: Select[T]

//   repeat.selectedItem.change.distinct foreach { optComp =>
//     input.value() = optComp match {
//       case opt: Option[T] => renderItem(opt.get)
//       case _ => ""
//     }
//   }
// }

// object ComboBox {
//   /**
//    * Creates an Combobox
//    * @tparam T the type of the items
//    * @param updateItems a function that deals with the String
//    * entered in the comboboxd input to update the list of items
//    * @param renderItem how to display items
//    */
//   def apply[T](
//     allItems: Seq[T],
//     renderItem: T => String)(
//       implicit observing: Observing,
//       config: CanRenderDomMutationConfig): ComboBox[T] =
//     new ComboBox[T](allItems, renderItem)(observing, config)

//   /**
//    * Creates an Combobox that uses the items' toString method to render them
//    * @tparam T the type of the items
//    * @param updateItems a function that deals with the String
//    * entered in the comboboxd input to update the list of items
//    */
//   def apply[T](allItems: Seq[T])(
//     implicit observing: Observing,
//     config: CanRenderDomMutationConfig): ComboBox[T] =
//     new ComboBox[T](allItems)(observing, config)

// }
