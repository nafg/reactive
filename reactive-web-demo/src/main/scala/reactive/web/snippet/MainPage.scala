package reactive
package web
package snippet

import _root_.scala.xml._

import net.liftweb.util.{Helpers, BindPlus}
  import Helpers._
  import BindPlus._
import net.liftweb.http._




// Among other things, ReactiveSnippet has an implicit
// val, currentPage, that among other things extends Observing.
// Thus any listeners we have can be garbage collected (only) once
// the snippet is garbage collected.
class MainPage extends ReactiveSnippet {
  //////////////////////////////////////////////////////////////////
  // DEMONSTRATE REACTIONS TO CLIENT EVENTS
  //////////////////////////////////////////////////////////////////
  
  // Create a reactive text input. By default its value is updated
  // when the browser fires a change event
  val field = TextInput()
  
  // Link the value property with the keyUp event
  // so that it's updated on every keyUp event
  field.value updateOn field.keyUp
  
  // Set its width
  field.size.value ()= 80

  
  // Create a Signal that binds the field's value
  // Its value will be kept up to date automatically
  val fieldValue = field.value.value map {v =>
    "*" #> Text(v)
  }
  
  // Create a NodeSeq=>NodeSeq that renders fieldValue
  // reactively in whatever element Cell is binded to.
  val cell = Cell(fieldValue)
  
  
  //////////////////////////////////////////////////////////////////
  // DEMONSTRATE REACTIONS TO SERVER EVENTS
  //////////////////////////////////////////////////////////////////
  
  // Create an EventStream that fires timer ticks until
  // the page is no longer alive
  val clockES = new Timer(interval = 2000, cancel = ()=> !isPageAlive)
  
  // Create a signal from the EventStream whose value, until
  // the first tick is received, is 0L
  val clockSig = clockES.hold(0L)
  
  // Create a reactive Span Cell that displays the time in a scala.xml.Text
  val clockSpan = Span(clockSig.map(t => Text((t/1000).toString)))
  

  //////////////////////////////////////////////////////////////////
  // DEMONSTRATE DELTA UPDATES
  //////////////////////////////////////////////////////////////////
  
  // Create an empty BufferSignal[Int]
  val items = BufferSignal[Int]()
  
  // Create a Repeater that binds each element in items
  // to the html element with id="number"
  // As numbers are inserted and removed from the BufferSignal,
  // the corresponding html elements will be inserted and removed.
  // This particular factory returns a NodeSeq=>NodeSeq that can
  // be used in Lift binding.
  val repeater: NodeSeq=>NodeSeq = Repeater {
    items.map{
      _ map { i =>
        ("#number" #> i) : (NodeSeq=>NodeSeq)
      }
    }
  }
  
  // Count from 1 to infinity
  var numbers = Stream.from(1)

  // On each clock tick do an insert or remove
  for(tick <- clockES) {
    println("Clock firing: " + tick)
    // If items is empty then always do an append
    // Otherwise do either an append or a remove,
    // depending on the value of math.random
    if(items.now.isEmpty || math.random > .4) {
      // Get the first number in the Stream and append it to items
      items.value += numbers.head
      // And point to the rest of the Stream
      numbers = numbers.tail
    } else {
      // Remove a random element from items
      items.value.remove(math.random*items.now.length toInt)
    }
  }
  
  
  /**
   * The snippet function
   */
  def render =
    "#field" #> field &
    "#span" #> cell &
    "#clock" #> clockSpan &
    "#div" #> repeater
}


/**
 * A bit more declarative
 */
class MainPage2 extends MainPage {
  override def render =
    "#field" #> field &
    "#span" #> Cell {
      field.value.value map {v => "*" #> Text(v) }
    } &
    "#clock" #> Span {
      clockSig map {t => Text(t/1000 toString)}
    } &
    "#div" #> Repeater {
      items map {
        _ map { i => ("#number" #> i) : (NodeSeq=>NodeSeq)}
      }
    }    
}
