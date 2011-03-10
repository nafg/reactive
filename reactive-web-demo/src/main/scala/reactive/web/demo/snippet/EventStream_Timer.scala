package reactive.web.demo.snippet

import reactive._


class EventStream_Timer extends Observing {
  // Create a timer that fires every 2 seconds, starting at 0,
  // for 30 seconds
  val timer = new Timer(0, 2000, {t =>  t >= 32000})
  
  def render = Demos.eventStreamOutput(
    for(t <- timer)
      yield "timer: " + t.toString
  )
}
