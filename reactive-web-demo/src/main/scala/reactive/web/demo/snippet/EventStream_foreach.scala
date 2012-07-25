package reactive
package web
package demo
package snippet

// all listeners added in here have the same gc lifetime as the class
// since it extends Observing
class EventStream_foreach extends PageSnippet {
  val eventSource = new EventSource[String] {}
  
  //The following is syntactic sugar for
  // eventSource.foreach(event => alert("You fired: '" + event + "'"))
  for(event <- eventSource) {
    alert("You fired: '" + event + "'")
  }
  
  def render = Demos.eventSourceInput(eventSource)
}
