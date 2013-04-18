package reactive
package web
package demo
package snippet

class EventStream_union extends PageSnippet {
  val eventSource1 = new EventSource[String] {}
  val eventSource2 = new EventSource[String] {}
  
  val eventStream = eventSource1 | eventSource2
  
  def render = Demos.eventSourceInput(eventSource1) ++
    Demos.eventSourceInput(eventSource2) ++
    Demos.eventStreamOutput(eventStream)
}
