package reactive.web.snippet

import reactive._


class EventStream_Timer extends EventStreamDemo {
  // Create a timer that fires every 2 seconds, starting at 0,
  // for 30 seconds
  // FIXME references this, to keep snippet from being gc'ed
  val timer = new Timer(0, 2000, {t => this; t > 30000})
  
  override lazy val eventStream = {
    eventSource |
      timer.map(t => "timer: " + t.toString)
  }
}
