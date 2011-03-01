package reactive.web.snippet

import scala.xml.{NodeSeq, Text}

import reactive._
  import web._

import net.liftweb.util._
  import Helpers._

object Demos {
  def eventSourceInput(eventSource: EventSource[String])(implicit o: Observing): NodeSeq = {
    val text = TextInput()
    val button = Button("Fire")(eventSource fire text.value.value.now)
    <xml:group>{text.render} {button.render}</xml:group>
  }
  def eventStreamOutput(eventStream: EventStream[String]): NodeSeq = Div {
    lazy val events = SeqSignal(
      eventStream.foldLeft(List[String]())((list, event) => event :: list).hold(Nil)
    )
    for (elements <- events) yield
      for (event <- elements) yield
        RElem(<p>Fired: '{ event }'</p>)
  }.render
}

