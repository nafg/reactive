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
trait Signal[+T] extends Forwardable[T] {
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
  def map[U, S](f: T => U)(implicit canMapSignal: CanMapSignal[U, S]): S = canMapSignal.map(this, f)

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
  /**
   * Returns a derived Signal that does not fire change events
   * during a prior call to fire on the same thread, thus
   * preventing infinite recursion between multiple signals
   * that are mutually dependent in an inconsistent manner.
   * For instance, if two Vars have a bidirectionally-enforced
   * mathematical relationship that can produce rounding errors.
   */
  def nonrecursive: Signal[T] = new NonrecursiveSignal[T](this)

  /**
   * Returns a derived Signal that only fires change events that are not equal to the
   * previous value, thus preventing infinite recursion between multiple signals
   * that are mutually dependent in a consistent manner.
   */
  def distinct: Signal[T] = new DistinctSignal[T](this)

  private type WithVolatility[T] = (T, () => Boolean)

  /**
   * Returns a derived signal in which value propagation does not happen on the thread triggering the change and block it.
   * This is ideal for handling values in ways that are time consuming.
   * The implementation delegates propagation to an actor (scala standard library).
   * The signal will hold values of type (T, ()=>Boolean), where T is the type of the
   * parent signal, and the value tuple will contain the parent's value at
   * the last time it was received, as well as a function that can be used to test
   * whether that value is outdated because the parent has received a new value.
   * This is because the actor implementation means that a new value cannot
   * be received until the previous value is finished being handled.
   * The test function is useful because it may be desirable to abort the time-consuming work
   * if the value has been superseded
   * Example usage:
   * for((v, isSuperseded) <- signal.nonblocking) { doSomework(); if(!isSuperseded()) doSomeMoreWork() }
   * If you don't care whether it was superseded just do
   * for((v, _) <- signal.nonblocking) ...
   */
  def nonblocking: Signal[(T, () => Boolean)] = new ChildSignal[T, WithVolatility[T], WithVolatility[T]](this, (now, new Volatility), identity) {
    import scala.actors.Actor._
    private val delegate = actor {
      loop {
        receive {
          case (x: T, volatility: Volatility) =>
            current = (x, volatility)
            change.fire((x, volatility))
        }
      }
    }
    def parentHandler = {
      case (parentEvent, (oldValue, volatility: Volatility)) =>
        volatility.stale = true
        val v = (parentEvent, new Volatility)
        delegate ! v
        v
    }
  }
}

private[reactive] class Volatility extends (() => Boolean) {
  @volatile private[reactive] var stale = false
  def apply() = stale
}

protected abstract class ChildSignal[T, U, S](protected val parent: Signal[T], protected var state: S, initial: S => U) extends Signal[U] {
  val change = new EventSource[U] {
    val ref = ph
  }
  protected var current = initial(state)
  def now = current

  protected def parentHandler: (T, S) => S
  private lazy val ph = parentHandler
  private val parentListener: T => Unit = x => synchronized {
    state = ph(x, state)
  }
  parent.change addListener parentListener
}

protected class MappedSignal[T, U](parent: Signal[T], f: T => U) extends ChildSignal[T, U, Unit](parent, (), _ => f(parent.now)) {
  def parentHandler = (x, _) => {
    val u = f(x)
    current = u
    change.fire(u)
    ()
  }
}

trait CanMapSignal[U, S] {
  def map[T](parent: Signal[T], f: T => U): S
}

trait LowPriorityCanMapSignalImplicits {
  implicit def canMapSignal[U]: CanMapSignal[U, Signal[U]] = new CanMapSignal[U, Signal[U]] {
    def map[T](parent: Signal[T], f: T => U): Signal[U] = new MappedSignal[T, U](parent, f)
  }
}
object CanMapSignal extends LowPriorityCanMapSignalImplicits {
  implicit def canMapSeqSignal[E]: CanMapSignal[TransformedSeq[E], SeqSignal[E]] = new CanMapSignal[TransformedSeq[E], SeqSignal[E]] {
    def map[T](parent: Signal[T], f: T => TransformedSeq[E]): SeqSignal[E] = new MappedSeqSignal[T, E](parent, f)
  }
}

trait CanFlatMapSignal[S1[T], S2[T]] {
  def flatMap[T, U](parent: S1[T], f: T => S2[U]): S2[U]
}

trait LowPriorityCanFlatMapSignalImplicits {
  implicit def canFlatMapSignal: CanFlatMapSignal[Signal, Signal] = new CanFlatMapSignal[Signal, Signal] {
    def flatMap[T, U](parent: Signal[T], f: T => Signal[U]): Signal[U] = new FlatMappedSignal[T, U](parent, f)
  }
}
object CanFlatMapSignal extends LowPriorityCanFlatMapSignalImplicits {
  implicit def canFlatMapSeqSignal: CanFlatMapSignal[Signal, SeqSignal] = new CanFlatMapSignal[Signal, SeqSignal] {
    def flatMap[T, U](parent: Signal[T], f: T => SeqSignal[U]): SeqSignal[U] = new FlatMappedSeqSignal[T, U](parent, f)
  }
}

protected class FlatMappedSignal[T, U](parent: Signal[T], f: T => Signal[U]) extends ChildSignal[T, U, Signal[U]](parent, f(parent.now), _.now) {
  private val thunk: U => Unit = x => synchronized {
    current = x
    change fire x
  }
  state.change addListener thunk
  def parentHandler = (x, curSig) => {
    curSig.change removeListener thunk
    val newSig = f(x)
    newSig.change addListener thunk
    change fire newSig.now
    current = newSig.now
    newSig
  }
}

protected class NonrecursiveSignal[T](parent: Signal[T]) extends ChildSignal[T, T, Unit](parent, (), _ => parent.now) {
  protected val changing = new scala.util.DynamicVariable(false)
  def parentHandler = (x, _) => {
    if (!changing.value) changing.withValue(true) {
      change fire x
    }
    ()
  }
}

protected class DistinctSignal[T](parent: Signal[T]) extends ChildSignal[T, T, Unit](parent, (), _ => parent.now) {
  var last = now
  def parentHandler = (x, _) => {
    if (x != last) {
      last = x
      change fire x
    }
    ()
  }
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

  def <-->(other: Var[T])(implicit observing: Observing): this.type = {
    this.distinct >> other
    other.distinct >> this
    this
  }

}

