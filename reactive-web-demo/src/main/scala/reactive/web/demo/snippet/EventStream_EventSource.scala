package reactive
package web
package demo
package snippet


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

trait EventStreamDemo extends PageSnippet {
  lazy val eventSource = new EventSource[String] {}
  lazy val eventStream: EventStream[String] = eventSource
  
  def render = Demos.eventSourceInput(eventSource) ++
    Demos.eventStreamOutput(eventStream)
}

