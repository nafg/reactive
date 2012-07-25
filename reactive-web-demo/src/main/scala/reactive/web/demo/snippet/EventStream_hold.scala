package reactive
package web
package demo
package snippet


class EventStream_hold extends PageSnippet {
  val eventSource = new EventSource[String] {}
  val signal = eventSource.hold("(initial value of signal)")
  
  def render = Demos.eventSourceInput(eventSource) ++
    <p>Signal value: '{Demos.signalOutput(signal)}'</p>
}
