package reactive
package web
package demo
package snippet

class EventStream_map extends EventStreamDemo {
  // Reverse the event
  override lazy val eventStream =
    eventSource.map(_.reverse)
}
