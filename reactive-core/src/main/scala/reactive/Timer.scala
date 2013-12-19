package reactive

import java.util.{Timer => juTimer, TimerTask}

import logging.Logger

private object _timer extends juTimer("reactive-core timer thread", true) {
  def scheduleAtFixedRate(delay: Long, interval: Long)(p: =>Unit): TimerTask = {
    val tt = new java.util.TimerTask {def run = p}
    super.scheduleAtFixedRate(tt, delay, interval)
    tt
  }
  def schedule(delay: Long)(p: =>Unit): TimerTask = {
    val tt = new java.util.TimerTask {def run = p}
    super.schedule(tt, delay)
    tt
  }
}

/**
 * An EventStream that fires events at the given interval.
 * Event values are (delta time in milliseconds) + startValue
 * There is no guarantee that the delta will be a multiple of interval, of course
 * Events are fired on a java.util.Timer thread
 * @param startValue the value this signal counts up from. Defaults to 0.
 * @param interval the frequency at which to update the signal's value. Defaults to 1 second.
 * @param until a function called with each tick that should return true to terminate the timer. By default it will never terminate.
 */
class Timer(
  private val startValue: Long = 0,
  interval: Long = 1000,
  until: Long=>Boolean = _ =>false
) extends EventSource[Long] {
  case class Canceling(tick: Long)
  
  private val origMillis = System.currentTimeMillis
  private val tt: TimerTask = _timer.scheduleAtFixedRate(interval, interval) {
    val tick = System.currentTimeMillis - origMillis + startValue
    if(until(tick)) {
      trace(Canceling(tick))
      tt.cancel
    } else {
      fire(tick)
    }
  }
}


/**
 * A Var that updates itself based on the supplied call-by-name
 * regularly, at a given interval, on a java.util.Timer thread.
 * @param interval the rate at which to update self
 * @param supplier a call-by-name that calculates the signal's value
 */
//TODO should this really extend Var?
class RefreshingVar[T](interval: Long)(supplier: =>T) extends Var(supplier) {
  _timer.scheduleAtFixedRate(interval, interval){value = supplier}
}
