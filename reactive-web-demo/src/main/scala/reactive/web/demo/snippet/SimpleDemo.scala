package reactive
package web
package demo
package snippet

import scala.xml._

import net.liftweb.util.Helpers._
import reactive.web.html._

// Extends Observing so any listeners we have can be garbage collected
// once the snippet is garbage collected, and not before.
class SimpleDemo extends PageSnippet {
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
  field.size () = Some(18)

  // Create a Signal that binds the field's value
  // Its value will be kept up to date automatically
  val fieldValue = field.value map { v => { _: NodeSeq => Text(v): NodeSeq }
  }

  // Create a NodeSeq=>NodeSeq that renders fieldValue
  // reactively in whatever element Cell is bound to.
  val cell = Cell(fieldValue)

  //////////////////////////////////////////////////////////////////
  // DEMONSTRATE REACTIONS TO SERVER EVENTS
  //////////////////////////////////////////////////////////////////

  // Create an EventStream that fires timer ticks for up to 10 minutes
  val clockES = new Timer(0, 2000, _ > 10.minutes)

  // Create a signal from the EventStream whose value, until
  // the first tick is received, is 0L
  val clockSig = clockES.hold(0L)

  // Create a reactive Span Cell that displays the time in a scala.xml.Text
  val clockSpan = Span(clockSig.map(t => Text((t / 1000).toString)))

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
  val repeater: NodeSeq => NodeSeq = Repeater {
    items.now.map{ i =>
      "#number" #> i: (NodeSeq => NodeSeq)
    }.signal
  }

  // Count from 1 to infinity
  var numbers = Stream.from(1)

  // On each clock tick do an insert or remove
  for (_ <- clockES) {
    // If items is empty then always do an append
    // Otherwise do either an append or a remove,
    // depending on the value of math.random
    if (items.now.isEmpty || math.random > .4) {
      // Get the first number in the Stream and append it to items
      items.value += numbers.head
      // And point to the rest of the Stream
      numbers = numbers.tail
    } else {
      // Remove a random element from items
      items.value.remove((math.random * items.now.length).toInt)
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
 * Same thing but a bit more declarative
 */
class SimpleDemo2 extends SimpleDemo {
  override def render =
    "#field" #> field &
      "#span" #> Cell {
        field.value map { v => "*" #> Text(v) }
      } &
      "#clock" #> Span {
        clockSig map { t => Text((t / 1000).toString) }
      } &
      "#div" #> Repeater {
        items.now.map { i => "#number" #> i: (NodeSeq => NodeSeq) }.signal
      }
}
