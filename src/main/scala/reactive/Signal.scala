package reactive

object Signal {
  //implicit def signalToEventStream[T](signal: SignalBase[T]): EventStreamBase[T] = signal.change
}
trait SignalBase[+T] {
  def now: T
  def change: EventStream[_ <: T]
  def foreach(f: T=>Unit)(implicit observing: Observing): Unit = change.foreach(f)(observing)
}
/**
 * A Signal in FRP represents a continuous value.
 * Here it is represented by the Signal trait, which is currently implemented in terms of a 'now' value
 * and a 'change' event stream. Transformations are implemented around those two members.
 * To obtain a signal, see Var, Val, Timer, and BufferSignal. In addition, new signals can be derived from
 * existing signals using the transformation methods defined in this trait.
 * @param T the type of value this signal contains
 */
//TODO transformations to not need to take an Observing--see parallel comment in EventStream
//TODO provide change veto (cancel) support
trait Signal[T] extends SignalBase[T] { parent =>
  protected class MappedSignal[U](f: T=>U)(implicit observing: Observing) extends Signal[U] {
    import scala.ref.WeakReference
    private val emptyCache = new WeakReference[Option[U]](None)
    protected var cached = emptyCache
    protected def cache(v: U): U = {
      cached = new WeakReference(Some(v))
      v
    }
    def now = cached.get match {
      case None | Some(None) =>
        cache(f(Signal.this.now))
      case Some(Some(ret)) => ret
    }
    /**
     * Fire change events whenever (the outer) Signal.this changes,
     * but the events should be transformed by f
     */
    lazy val change = Signal.this.change.map(f)

    //TODO we need a way to be able to do this only if there are no real listeners
    for(v <- change) cache(v)
  }
  
  /**
   * Represents the current value. Often, this value does not need to be
   * (or should not be) used explicitly from the outside; instead you can pass functions
   * that operate on the value, to the Signal.
   */
  def now: T
  
  /**
   * Returns an EventStream that, every time this signal's value changes, fires
   * an event consisting of the new value.
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
  def map[U](f: T=>U)(implicit observing: Observing) = new MappedSignal[U](f)
  
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
    //TODO cache
    def now = f(parent.now).now
    val change = parent.change.flatMap(parent.now){_ => f(parent.now).change}(observing)
    parent.change foreach {_ =>
      println("Parent change, firing " + now)
      change fire now
    }
  }
  //TODO differentiate types at runtime rather than compile time
  def flatMap[U](f: T => SeqSignal[U])(implicit o: Observing) = new SeqSignal[U] {
    //TODO cache
    def now = f(parent.now).now
    def observing = o
    override lazy val change = parent.change.flatMap(parent.now){_ => f(parent.now).change}(observing)
    parent.change.foldLeft[Seq[Message[T,U]]](Nil){(prev: Seq[Message[T,U]], cur: T) =>
      val n = f(cur)
      change fire n.transform
      println(n.transform.getClass)
      val (da, db) = (prev, Batch(n.transform.baseDeltas.map{_.asInstanceOf[Message[T,U]]}: _*).flatten)
      val toUndo = da.filterNot(db.contains) map {_.inverse} reverse
      val toApply = db.filterNot(da.contains)
      println("toUndo: "  + toUndo)
      println("toApply: "  + toApply)
      deltas fire Batch(toUndo ++ toApply map {_.asInstanceOf[Message[U,U]]}: _*)
      db
    }
    override lazy val deltas = parent.change.flatMap(parent.now){_ => f(parent.now).deltas}(observing)
  }
}


/**
 * A signal representing a value that never changes
 * (and hence never firees change events)
 */
case class Val[T](now: T) extends Signal[T] {
  def change = new EventStream[T] {}
}

/**
 * Defines a factory for Vars
 */
object Var {
  def apply[T](v: T) = new Var(v)
}
/**
 * A signal whose value can be changed directly
 */
class Var[T](initial: T) extends Signal[T] {
  private var _value = initial
  def now = value
  //TODO do we need value? why not just now and now_= ? Or just now and update?
  def value = _value
  /**
   * Setter. Usage: var.value = x
   */
  def value_=(v: T) {
    _value = v
    change.fire(v)
  }
  /**
   * Usage: var()=x
   */
  final def update(v: T) = value = v
  
  /**
   * Fires an event after every mutation, consisting of the new value
   */
  lazy val change = new EventStream[T] {}
}

private object _timer extends java.util.Timer {
  def scheduleAtFixedRate(delay: Long, interval: Long)(p: =>Unit) =
    super.scheduleAtFixedRate(new java.util.TimerTask {def run = p}, delay, interval)
}

/**
 * A signal whose value represents elapsed time in milliseconds, and is updated
 * on a java.util.Timer thread.
 * @param startTime the value this signal counts up from
 * @param interval the frequency at which to update the signal's value.
 */
//TODO should this really extend Var?
//TODO could/should this be implemented as a RefreshingVar?
class Timer(private val startTime: Long = 0, interval: Long) extends Var(startTime) {
  private val origMillis = System.currentTimeMillis
  _timer.scheduleAtFixedRate(interval, interval){
    value = System.currentTimeMillis - origMillis + startTime
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
