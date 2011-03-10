package reactive.web.demo.snippet

import reactive._

class EventStream_hold extends Observing {
  val eventSource = new EventSource[String] {}
  val signal = eventSource.hold("(initial value of signal)")
  
  def render = Demos.eventSourceInput(eventSource) ++
    <p>Signal value: '{Demos.signalOutput(signal)}'</p>
}
