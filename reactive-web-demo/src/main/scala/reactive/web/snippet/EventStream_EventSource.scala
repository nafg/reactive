package reactive.web.snippet

import scala.xml.NodeSeq

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
  val text = TextInput()
  lazy val eventSource = new EventSource[String] {}
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
