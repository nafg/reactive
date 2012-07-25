package reactive
package web
package demo
package snippet

//lazy, singleton; never stops, and shared by all
object timer extends Timer(System.currentTimeMillis, 2000)


class EventStream_flatMap extends EventStreamDemo {
  val seconds = timer.map(_ / 1000 % 100000)
  // New EventStream based on input event
  // enter > 5 chars to count down
  override lazy val eventStream =
    eventSource.flatMap(s =>
      if(s.length > 5) {
        val c = System.currentTimeMillis % 100000
        seconds.map(t => s + " at " + (c-t))
      } else
        seconds.map(s + " at " + _)
    )
  
  // not actually being used
  def equivalentWithSugar = for {
    s <- eventSource
    t <- seconds
  } yield s + " at " + t
}
