package reactive
package web
package demo
package snippet

class EventStream_filter extends EventStreamDemo {
  // Only allow short events
  override lazy val eventStream =
    eventSource.filter(_.length < 5)
}
