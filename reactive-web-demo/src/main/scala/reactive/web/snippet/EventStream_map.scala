package reactive.web.snippet

import reactive._


class EventStream_map extends EventStreamDemo {
  // Reverse the event
  override lazy val eventStream =
    eventSource.map(_.reverse)
}
