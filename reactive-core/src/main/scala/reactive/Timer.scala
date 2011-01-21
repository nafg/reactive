package reactive

private object _timer extends java.util.Timer {
  def scheduleAtFixedRate(delay: Long, interval: Long)(p: =>Unit) =
    super.scheduleAtFixedRate(new java.util.TimerTask {def run = p}, delay, interval)
}

/**
 * An EventStream that fires events at the given interval.
 * Event values are (delta time in milliseconds) + startValue
 * There is no guarantee that the delta will be a multiple of interval, of course
 * Events are fired on a java.util.Timer thread
 * @param startTime the value this signal counts up from
 * @param interval the frequency at which to update the signal's value.
 */
class Timer(private val startValue: Long = 0, interval: Long) extends EventSource[Long] {
  private val origMillis = System.currentTimeMillis
  _timer.scheduleAtFixedRate(interval, interval){
    fire(System.currentTimeMillis - origMillis + startValue)
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
