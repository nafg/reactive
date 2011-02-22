package reactive.web.snippet

import reactive._


class EventStream_Timer extends EventStreamDemo {
  // Create a timer that fires every 1/2 second, starting at 0,
  // for 30 seconds
  val timer = new Timer(0, 500, _ > 30000)
  
  override lazy val eventStream = {
    eventSource |
      timer.map(t => "timer: " + t.toString)
  }
}
