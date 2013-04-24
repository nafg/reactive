package reactive
package web
package demo
package snippet

import scala.xml.{ NodeSeq, Text }

import reactive.web.html._

import net.liftweb.util._
import Helpers._

object Demos {
  def eventSourceInput(eventSource: EventSource[String])(implicit o: Observing, page: Page): NodeSeq = {
    val text = TextInput()
    val button = Button("Fire")(eventSource fire text.value.now)
    <xml:group>{ text.render } { button.render }</xml:group>
  }

  def eventStreamOutput(eventStream: EventStream[String])(implicit page: Page): NodeSeq = Div {
    lazy val events = SeqSignal(
      eventStream.foldLeft(List[String]())((list, event) => event :: list).hold(Nil)
    )
    events.now.map{ e => RElem(<p>Fired: '{ e }'</p>) }.signal
  }.render

  def varInput(v: Var[String])(implicit o: Observing, page: Page): NodeSeq = {
    val textInput = TextInput(v)
    textInput.value updateOn textInput.keyUp
    textInput.render
  }

  def signalOutput(signal: Signal[String])(implicit page: Page): NodeSeq = Span {
    signal map Text.apply
  }.render
}

