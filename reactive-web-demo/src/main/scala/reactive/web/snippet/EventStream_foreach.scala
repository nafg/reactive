package reactive.web.snippet

import reactive._
  import web.alert

// all listeners added in here have the same gc lifetime as the class
// since it extends Observing
class EventStream_foreach extends Observing {
  val eventSource = new EventSource[String] {}
  
  //The following is syntactic sugar for
  // eventSource.foreach(event => alert("You fired: '" + event + "'"))
  for(event <- eventSource) {
    alert("You fired: '" + event + "'")
  }
  
  def render = Demos.eventSourceInput(eventSource)
}
