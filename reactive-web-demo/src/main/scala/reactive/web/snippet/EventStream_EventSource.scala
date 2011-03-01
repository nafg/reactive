package reactive.web.snippet

import scala.xml.{NodeSeq, Text}

import reactive._
  import web._

import net.liftweb.util._
import Helpers._

class EventStream_EventSource extends EventStreamDemo {
  override lazy val eventSource = {
    val es = new EventSource[String] {}
    scala.concurrent.ops.spawn {
      Thread.sleep(10000)
      es fire "Event after 10 seconds"
      for(i <- 1 to 3) {
        Thread.sleep(5000)
        es.fire("Event after 5 seconds #" + i.toString)
      }
    }
    es
  }

}

trait EventStreamDemo extends Observing {
  lazy val eventSource = new EventSource[String] {}
  lazy val eventStream: EventStream[String] = eventSource
  
  def render = Demos.eventSourceInput(eventSource) ++
    Demos.eventStreamOutput(eventStream)
}

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

