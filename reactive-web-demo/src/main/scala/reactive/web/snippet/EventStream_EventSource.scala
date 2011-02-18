package reactive.web.snippet

import scala.xml.NodeSeq

import reactive._
import web._

import net.liftweb.util._
import Helpers._

class EventStream_EventSource extends EventStreamDemo with ReactiveSnippet {
  override val eventSource = new EventSource[String] {}

  override def render = {
    scala.concurrent.ops.spawn {
      Thread.sleep(10000)
      eventSource fire "Event after 10 seconds"
      (1 to 3) map (_.toString) foreach { s =>
        Thread.sleep(5000)
        eventSource.fire("Event after 5 seconds #" + s)
      }
    }
    super.render
  }
}

trait EventStreamDemo { this: ReactiveSnippet =>
  val text = TextInput()
  val eventSource = new EventSource[String] {}
  lazy val eventStream: EventStream[String] = eventSource
  lazy val events = SeqSignal(
    eventStream.foldLeft(List[String]())((list, event) => event :: list).hold(Nil))
  def render = "input" #> text &
    "button" #> Button("Fire")(eventSource fire text.value.value.now) &
    ".div" #> Div {
      for (elements <- events) yield
        for (event <- elements) yield
          RElem(<p>Fired: '{ event }'</p>)
    }
}
