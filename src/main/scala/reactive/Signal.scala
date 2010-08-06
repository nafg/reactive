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
*/
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


case class Val[T](now: T) extends Signal[T] {
  def change = new EventStream[T] {}
}

object Var {
  def apply[T](v: T) = new Var(v)
}
class Var[T](initial: T) extends Signal[T] {
  private var _value = initial
  def now = value
  def value = _value
  def value_=(v: T) {
    _value = v
    change.fire(v)
  }
  final def update(v: T) = value = v
  
  lazy val change = new EventStream[T] {}
}

private object _timer extends java.util.Timer {
  def scheduleAtFixedRate(delay: Long, interval: Long)(p: =>Unit) =
    super.scheduleAtFixedRate(new java.util.TimerTask {def run = p}, delay, interval)
}

class Timer(private val startTime: Long = 0, interval: Long) extends Var(startTime) {
  private val origMillis = System.currentTimeMillis
  _timer.scheduleAtFixedRate(interval, interval){
    value = System.currentTimeMillis - origMillis + startTime
  }
}

class RefreshingVar[T](interval: Long)(supplier: =>T) extends Var(supplier) {
  _timer.scheduleAtFixedRate(interval, interval){value = supplier}
}
