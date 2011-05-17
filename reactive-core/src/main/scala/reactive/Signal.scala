package reactive

/**
 * A Signal in FRP represents a continuous value.
 * 
 * Here it is represented by the Signal trait, which is currently implemented in terms of a 'now' value
 * and a 'change' event stream. Transformations are implemented around those two members.
 * 
 * To obtain a signal, see Var, Val, Timer, and BufferSignal. In addition, new signals can be derived from
 * existing signals using the transformation methods defined in this trait.
 * @param T the type of value this signal contains
 */
//TODO provide change veto (cancel) support
trait Signal[+T] {
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
  def foreach(f: T => Unit)(implicit observing: Observing): Unit = {
    f(now)
    change.foreach(f)(observing)
  }
  
  /**
   * Return a new Signal whose value is computed from the value
   * of this Signal, transformed by f. It fires change events
   * whenever (and only when) the original Signal does, but the
   * event values are transformed by f.
   * 
   * For example:<pre> 
   * val a: Signal[Int] = ...
   * val b = a.map(_ + 1)
   * </pre>
   * b represents a Signal whose value is always 1 greater than a.
   * Whenever a fires an event of x, b fires an event of x+1.
   */
  def map[U, S](f: T=>U)(implicit canMapSignal: CanMapSignal[U,S]): S = canMapSignal.map(this, f)

  /**
   * Returns a new signal, that for every value of this parent signal,
   * will correspond to the signal resulting from applying f to
   * the respective value of this parent signal.
   * Whenever this Signal's change EventStream fires, the
   * resulting Signal's change EventStream will fire the
   * value of the new signal, and subsequently will fire
   * all change events fired by that signal.
   * This can be used to build a signal that switches
   * among several other signals.
   * For example:
   * val sa: Signal[Int] = ...
   * def sb(a: Int): Signal[Int] = a.map(_ + 1)
   * val sc = sa.flatMap(a => sb(a))
   * 
   * If the function is typed to return a SeqSignal, its deltas and changes correspond to
   * those of the SeqSignals returned by ''f'', after each invocation
   * of ''f''.
   * In addition, every change to the parent results in a change event
   * as well as deltas reflecting the transition from the SeqSignal
   * previously returned by ''f'' and the on returned by it now.
   */
  //TODO perhaps allow for flatMap(T => EventStream[U]), i.e., S==EventStream?
  def flatMap[U, S[X]](f: T => S[U])(implicit canFlatMapSignal: CanFlatMapSignal[Signal, S]): S[U] = canFlatMapSignal.flatMap(this, f)

  /**
   * Returns a Tuple2-valued Signal that contains the values of this Signal and another Signal
   * @param that the other Signal
   * @return the Tuple2-valued Signal
   */
  def zip[U](that: Signal[U]): Signal[(T, U)] = this flatMap { v1 =>
    that map { v2 =>
      (v1, v2)
    }
  }
}




protected class MappedSignal[T,U](private val parent: Signal[T], f: T=>U) extends Signal[U] {
  import scala.ref.WeakReference
  private val emptyCache = new WeakReference[Option[U]](None)
  protected var cached = emptyCache
  protected val cache = {v: U =>
    cached = new WeakReference(Some(v))
  }
  def now = cached.get match {
    case None | Some(None) =>
      val ret = f(parent.now)
      cache(ret)
      ret
    case Some(Some(v)) =>
      v
  }
  
  /**
   * Fire change events whenever (the outer) Signal.this changes,
   * but the events should be transformed by f
   */
  lazy val change = parent.change.map(f)

  //TODO we need a way to be able to do this only if there are no real listeners
  change addListener cache
}

trait CanMapSignal[U, S] {
  def map[T](parent: Signal[T], f: T=>U): S
}

trait LowPriorityCanMapSignalImplicits {
  implicit def canMapSignal[U]: CanMapSignal[U, Signal[U]] = new CanMapSignal[U, Signal[U]] {
    def map[T](parent: Signal[T], f: T=>U): Signal[U] = new MappedSignal[T,U](parent,f)
  }
}
object CanMapSignal extends LowPriorityCanMapSignalImplicits {
  implicit def canMapSeqSignal[E]: CanMapSignal[TransformedSeq[E], SeqSignal[E]] = new CanMapSignal[TransformedSeq[E], SeqSignal[E]] {
    def map[T](parent: Signal[T], f: T=>TransformedSeq[E]): SeqSignal[E] = new MappedSeqSignal[T,E](parent,f)
  }
}

trait CanFlatMapSignal[S1[T], S2[T]] {
  def flatMap[T, U](parent: S1[T], f: T => S2[U]): S2[U]
}

trait LowPriorityCanFlatMapSignalImplicits {
  implicit def canFlatMapSignal: CanFlatMapSignal[Signal, Signal] = new CanFlatMapSignal[Signal, Signal] {
    def flatMap[T, U](parent: Signal[T], f: T=>Signal[U]): Signal[U] = new FlatMappedSignal[T,U](parent,f)
  }
}
object CanFlatMapSignal extends LowPriorityCanFlatMapSignalImplicits {
  implicit def canFlatMapSeqSignal: CanFlatMapSignal[Signal, SeqSignal] = new CanFlatMapSignal[Signal, SeqSignal] {
    def flatMap[T, U](parent: Signal[T], f: T=>SeqSignal[U]): SeqSignal[U] = new FlatMappedSeqSignal[T,U](parent,f)
  }
}


protected class FlatMappedSignal[T,U](private val parent: Signal[T], f: T=>Signal[U]) extends Signal[U] {
  def now = currentMappedSignal.now
  // We send out own change events as we are an aggregate signal
  val change = new EventSource[U] {}
  private val listener: U => Unit = { x => change fire x}
  // Currently mapped value
  private var currentMappedSignal = f(parent.now)
  // Register our first listener
  currentMappedSignal.change addListener listener
  // When the parent changes, we need to update our forwarding listeners and send the new state of this aggregate signal.
  private val parentListener = { x: T =>
    currentMappedSignal.change removeListener listener
    currentMappedSignal = f(x)
    currentMappedSignal.change addListener listener
    listener(now)
  }
  parent.change addListener parentListener
}



/**
 * A signal representing a value that never changes
 * (and hence never fires change events)
 */
case class Val[T](now: T) extends Signal[T] {
  def change = new EventSource[T] {}
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
  //Advantage of setter other than update is to allow for += type assignments
  // 'var.value += 2' works; 'var ()+= 2' does not work.
  def value = _value
  /**
   * Setter. Usage: var.value = x
   */
  def value_=(v: T) {
    _value = v
    change0.fire(v)
  }
  /**
   * Usage: var()=x
   */
  final def update(v: T) = value = v
  
  /**
   * Fires an event after every mutation, consisting of the new value
   */
  lazy val change: EventStream[T] = change0
  private lazy val change0 = new EventSource[T] {}

  override def toString = "Var("+now+")"  
}

