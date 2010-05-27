package reactive

object Signal {
  implicit def signalToEventStream[T](signal: Signal[T]): EventStream[T] = signal.change
}
/**
 * A Signal in FRP represents a continuous value.
*/
trait Signal[T] {
  /**
   * Used to store the current value. This value should not be
   * used from the outside in many cases; rather, pass functions
   * that operate on the value to the Signal. 
   */
  def now: T
  
  /**
   * Returns an EventStream that represents the value every time
   * it changes.
   */
  def change: EventStream[T]
  
  /**
   * Return a new Signal whose value is computed from the value
   * of this Signal, transformed by f. It fires change events
   * whenever (and only when) the original Signal does, but the
   * event values are transformed by f.
   * For example:
   * val a: Signal[Int] = ...
   * val b = a.map(_ + 1)
   * b represents a Signal whose value is always 1 greater than a.
   * Whenever a fires an event of x, b fires an event of x+1.
   */
  def map[U](f: T=>U)(implicit observing: Observing) = new Signal[U] {
    // this could be done more efficiently by saving
    // the new value of value after every change event
    private val emptyCache = new scala.ref.WeakReference[Option[U]](None)
    private var cached = emptyCache
    def now = cached.get match {
      case None | Some(None) =>
        val ret = f(Signal.this.now)
        cached = new scala.ref.WeakReference(Some(ret))
        ret
      case Some(Some(ret)) => ret
    }
    for(_ <- change) cached = emptyCache
    /**
     * Fire change events whenever (the outer) Signal.this changes,
     * but the events should be transformed by f
     */
    def change = Signal.this.change.map(f)
  }
  
  /**
   * Combine two Signals to form a new composite Signal.
   * The new Signal has the value of the Signal calculated
   * by applying f to this Signal's value.
   * Whenever this Signal's change EventStream fires, the
   * resulting Signal's change EventStream will corresponds
   * to the calculated Signal's change EventStream.
   * For example:
   * val sa: Signal[Int] = ...
   * def sb(a: Int): Signal[Int] = a.map(_ + 1)
   * val sc = sa.flatMap(a => sb(a))
   */
  def flatMap[U](f: T => Signal[U])(implicit observing: Observing) = new Signal[U] {
    def now = f(Signal.this.now).now
    val change = Signal.this.change.flatMap(Signal.this.now){_ => f(Signal.this.now).change}(observing)
  }
}


case class Val[T](now: T) extends Signal[T] {
  def change = new EventStream[T] {}
}

object Var {
  def apply[T](v: T) = new Var(v)
}
class Var[T](initial: T) extends Signal[T] {
  private var _value = initial
  def now = _value
  final def value = now
  def value_=(v: T) {
    _value = v
    change.fire(v)
  }
  final def update(v: T) = value = v
  
  val change = new EventStream[T] {}
}

private object timer extends java.util.Timer

class Clock_(private val startTime: Long = 0, interval: Long) extends Var(startTime) {
  private val origMillis = System.currentTimeMillis
  timer.scheduleAtFixedRate(
    new java.util.TimerTask {
      def run = value = System.currentTimeMillis - origMillis + startTime 
    },
    0,
    interval
  )
}
