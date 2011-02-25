package reactive.web.snippet

import reactive._


class EventStream_filter extends EventStreamDemo {
  // Only allow short events
  override lazy val eventStream =
    eventSource.filter(_.length < 5)
}
