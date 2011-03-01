package reactive.web.snippet

import reactive._


class EventStream_Timer extends Observing {
  // Create a timer that fires every 2 seconds, starting at 0,
  // for 30 seconds
  // FIXME references this, to keep snippet from being gc'ed
  val timer = new Timer(0, 2000, {t => this; t > 30000})
  
  def render = Demos.eventStreamOutput(
    for(t <- timer)
      yield "timer: " + t.toString
  )
}
