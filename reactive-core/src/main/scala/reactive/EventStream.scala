package reactive

import scala.ref.WeakReference
import scala.util.DynamicVariable
import scala.annotation.tailrec

object EventSource {
  @deprecated("Use Logger.defaultLevel, this does nothing anymore")
  var debug = false
}

object EventStream {
  private object empty0 extends EventSource[Nothing] {
    override def debugName = "EventStream.empty"
  }
  def empty[A]: EventStream[A] = empty0
}

/**
 * An EventStream is a source of events (arbitrary values sent to listener functions).
 * You can fire events from it, you can react to events with any behavior, and you can
 * create derived EventStreams, whose events are based on the original EventStream.
 * The API is modeled after the Scala standard library collections framework.
 *
 * An EventStream is like a collection in the sense that it consists of multiple values.
 * However, unlike actual collections, the values are not available upon request; they
 * occur whenever they occur. Nevertheless, many operations that apply to collections
 * apply to event streams. To react to events, use foreach or foldLeft. To create derived,
 * transformed EventStreams, use map, flatMap, filter, foldLeft, and the | (union) operator.
 * Note that you can of course use for comprehensions as syntactic sugar for many
 * of the above.
 *
 * Methods that return a new EventStream generally do not require an (implicit) Observing.
 * Instead, the new EventStream itself holds references to the parent EventStream and
 * the event function (which refers to, or is, the function passed in to the method).
 * (As a result, if you derive EventStreams with a function that performs side effects,
 * in order to ensure that the function is not garbage collected you must retain a reference
 * to the resulting EventStream.)
 *
 * On the other hand, methods which do require an Observing take functions which are expected
 * perform side effects, and therefore do not hold a reference to the function themselves but
 * rather use the Observing for that purpose. As a result, they will remain in memory as long
 * as the Observing object does.
 *
 * You can also create a Signal from an EventStream using hold.
 * @tparam T the type of values fired as events
 * @see EventSource
 */
trait EventStream[+T] extends Forwardable[T, EventStream[T]] {
  def self = this

  /**
   * Registers a listener function to run whenever
   * an event is fired. The function is held with a WeakReference
   * and a strong reference is placed in the Observing, so
   * the latter determines the function's gc lifetime.
   * @param f a function to be applied on every event
   * @param observing the object whose gc lifetime should determine that of the function
   */
  def foreach(f: T => Unit)(implicit observing: Observing): Unit
  /**
   * Returns a new EventStream, that for every event that this EventStream
   * fires, that one will fire an event that is the result of
   * applying 'f' to this EventStream's event.
   * @param f the function that transforms events fired by this EventStream
   * into events to be fired by the resulting EventStream.
   */
  def map[U](f: T => U): EventStream[U]
  /**
   * Create a new EventStream that consists of the events of
   * the EventStreams returned by f. f is applied on every
   * event of the original EventStream, and its returned
   * EventStream is used until the next event fired by
   * the original EventStream, at which time the previously
   * returned EventStream is no longer used and a new one
   * is used instead.
   * @param f the function that is applied for every event
   * to produce the next segment of the resulting EventStream.
   */
  def flatMap[U](f: T => EventStream[U]): EventStream[U]
  /**
   * Returns a new EventStream that propagates a subset of the events that
   * this EventStream fires.
   * @param f the predicate function that determines which events will
   * be fired by the new EventStream.
   */
  def filter(f: T => Boolean): EventStream[T]
  /**
   * Filter and map in one step. Takes a PartialFunction.
   * Whenever an event is received, if the PartialFunction
   * is defined at that event, the value returned by applying
   * it will be fired.
   * @param f the PartialFunction
   */
  def collect[U](pf: PartialFunction[T, U]): EventStream[U]
  /**
   * Returns a new EventStream that propagates this EventStream's events
   * until the predicate returns false.
   * @param p the precate function, taking an event as its argument
   * and returning true if event propagation should continue
   */
  def takeWhile(p: T => Boolean): EventStream[T]
  /**
   * Allows one, in a functional manner, to respond to an event while
   * taking into account past events.
   * For every event t, f is called with arguments (u, t), where u
   * is initially the value of the 'initial' parameter, and subsequently
   * the result of the previous application of f.
   * Returns a new EventStream that, for every event t fired by
   * the original EventStream, fires the result of the application of f
   * (which will also be the next value of u passed to it).
   * Often 'u' will be an object representing some accumulated state.
   * For instance, given an EventStream[Int] named 'es',
   * es.foldLeft(0)(_ + _)
   * would return an EventStream that, for every (integer) event fired
   * by es, would fire the sum of all events that have been fired by es.
   */
  def foldLeft[U](initial: U)(f: (U, T) => U): EventStream[U]
  /**
   * Union of two EventStreams.
   * Returns a new EventStream that consists of all events
   * fired by both this EventStream and 'that.'
   * @param that the other EventStream to combine in the resulting
   * EventStream.
   */
  def |[U >: T](that: EventStream[U]): EventStream[U]
  /**
   * Returns a Signal whose value is initially the 'init' parameter,
   * and after every event fired by this EventStream, the value of
   * that event.
   * @param init the initial value of the signal
   */
  def hold[U >: T](init: U): Signal[U]

  /**
   * Returns a derived EventStream that does not fire events
   * during a prior call to fire on the same thread, thus
   * preventing infinite recursion between multiple event streams
   * that are mutually dependent.
   */
  def nonrecursive: EventStream[T]

  /**
   * Returns a derived EventStream that only fires events that are not equal to the
   * previous event. This can be used to prevent infinite recursion between multiple
   * event streams that are mutually dependent in a consistent manner.
   */
  def distinct: EventStream[T]

  /**
   * Returns a derived event stream in which event propagation does not happen on the thread firing it and block it.
   * This is helpful when handling events can be time consuming.
   * The implementation delegates propagation to an actor (scala standard library), so
   * events are handled sequentially.
   */
  def nonblocking: EventStream[T]

  /**
   * Returns an EventStream whose tuple-valued events include a function for testing staleness.
   * The events will be of type (T, ()=>Boolean), where T is the type of the
   * parent event stream; and the tuple will contain the event fired in the parent
   * as well as a function that can be used to test
   * whether that event is outdated because a new event has been fired since then.
   * This is especially useful in conjunction with 'nonblocking',
   * because its actor implementation means that a new event cannot
   * be received until the previous event is finished being handled.
   * The test function is useful because it may be desirable to abort the time-consuming work
   * if a new event has been fired since then.
   * Example usage:
   * for((v, isSuperseded) <- eventStream.zipWithStaleness) { doSomework(); if(!isSuperseded()) doSomeMoreWork() }
   */
  def zipWithStaleness: EventStream[(T, () => Boolean)]

  /**
   * Returns an EventStream that only fires events that are not
   * followed by another event within ''period'' milliseconds.
   * For instance, if you want to display some results in response to
   * the user typing, and you do not want to perform more work than
   * necessary, you may want to wait until the user has not typed
   * anything for a full second.
   */
  def throttle(period: Long): EventStream[T]

  private[reactive] def addListener(f: (T) => Unit): Unit
  private[reactive] def removeListener(f: (T) => Unit): Unit

  def debugString: String
  def debugName: String
}

class NamedFunction[-T, +R](name: => String, f: T => R) extends (T => R) {
  def apply(in: T): R = f(in)
  override def toString = "%s #%s" format (name, System.identityHashCode(f))
}
object NamedFunction {
  def apply[T, R](name: => String)(f: T => R) = new NamedFunction(name, f)
}

/**
 * A basic implementation of EventStream,
 * adds fire method.
 */
//TODO perhaps EventSource = SimpleEventStream + fire
class EventSource[T] extends EventStream[T] with Forwardable[T, EventSource[T]] with Logger {
  case class HasListeners(listeners: List[WeakReference[T => Unit]])
  case class FiringEvent(event: T, listenersCount: Int, collectedCount: Int)
  case class AddingListener(listener: T => Unit)
  case class AddedForeachListener(listener: T => Unit)

  @deprecated("Use logLevel or setLogLevel, this does nothing anymore")
  var debug = EventSource.debug

  override def self = this

  /**
   * When n empty WeakReferences are found, purge them
   */
  protected val purgeThreshold = 10

  abstract class ChildEventSource[U, S](protected var state: S) extends EventSource[U] {
    private val parent = EventSource.this
    protected def handler: (T, S) => S
    private val h = handler
    protected val listener: T => Unit = NamedFunction(debugName+".listener")(v => synchronized {
      state = h(v, state)
    })
    parent addListener listener
  }

  class FlatMapped[U](initial: Option[T])(val f: T => EventStream[U]) extends ChildEventSource[U, Option[EventStream[U]]](initial map f) {
    override def debugName = "%s.flatMap(%s)" format (EventSource.this.debugName, f)
    val thunk: U => Unit = fire _
    state foreach { _ addListener thunk }
    def handler = (parentEvent, lastES) => {
      lastES foreach { _ removeListener thunk }
      val newES = Some(f(parentEvent))
      newES foreach { _ addListener thunk }
      newES
    }
  }

  class Throttled(delay: Long) extends ChildEventSource[T, (Option[T], Long)](None -> System.currentTimeMillis) {
    override def debugName = EventSource.this.debugName+".throttle("+delay+")"
    private def onTimer {
      val t1 = System.currentTimeMillis
      state._1 match {
        case Some(e) => fire(e)
        case _       =>
      }
      state = None -> t1
    }
    var tt = _timer.schedule(delay)(onTimer)
    def handler = {
      case (event, _) =>
        tt.cancel()
        tt = _timer.schedule(delay)(onTimer)
        Some(event) -> System.currentTimeMillis
    }
  }

  class FoldedLeft[U](initial: U, f: (U, T) => U) extends ChildEventSource[U, U](initial) {
    override def debugName = "%s.foldLeft(%s)(%s)" format (EventSource.this.debugName, initial, f)
    def handler = (event, last) => {
      val next = f(last, event)
      fire(next)
      next
    }
  }

  class Collected[U](pf: PartialFunction[T, U]) extends ChildEventSource[U, Unit] {
    override def debugName = "%s.collect(%s)" format (EventSource.this.debugName, pf)
    private val pf0 = pf
    def handler = (event, _) => {
      if (pf.isDefinedAt(event))
        fire(pf apply event)
    }
  }

  private type WithVolatility[T] = (T, () => Boolean)

  class ActorEventStream extends ChildEventSource[T, Unit](()) {
    override def debugName = "%s.nonblocking" format (EventSource.this.debugName)
    import scala.actors.Actor._
    private val delegate = actor {
      loop {
        receive {
          case x: T => fire(x)
        }
      }
    }
    def handler = {
      case (parentEvent, _) =>
        delegate ! parentEvent
    }
  }

  private var listeners: List[WeakReference[T => Unit]] = Nil

  /**
   * Whether this EventStream has any listeners depending on it
   */
  //TODO should it return false if it has listeners that have been gc'ed?
  def hasListeners = listeners.nonEmpty //&& listeners.forall(_.get.isDefined)

  def debugName = "(eventSource: %s #%s)".format(getClass, System.identityHashCode(this))
  def debugString = {
    "%s\n  listeners%s".format(
      debugName,
      listeners.flatMap(_.get).mkString("\n    ", "\n    ", "\n")
    )
  }

  /**
   * Sends an event to all listeners.
   * @param event the event to send
   */
  def fire(event: T) {
    trace(
      FiringEvent(
        event,
        listeners.size,
        listeners.length - listeners.count(_.get ne None)
      )
    )
    trace(HasListeners(listeners))
    var empty = 0
    @tailrec def next(ls: List[WeakReference[T => Unit]]): Unit = ls match {
      case Nil =>
      case xs =>
        val l = xs.head.get
        if (l.isDefined) l.get apply event else empty += 1
        next(ls.tail)
    }
    next(listeners)
    if (empty >= purgeThreshold) listeners = listeners.filter(_.get.isDefined)
  }

  def flatMap[U](f: T => EventStream[U]): EventStream[U] =
    new FlatMapped(None)(f)

  @deprecated("Use eventStream.hold(initial).flatMap(f)")
  def flatMap[U](initial: T)(f: T => EventStream[U]): EventStream[U] =
    new FlatMapped(Some(initial))(f) {
      override def debugName = "%s.flatMap(%s)(%s)" format (EventSource.this.debugName, initial, f)
    }

  def collect[U](pf: PartialFunction[T, U]): EventStream[U] = new Collected(pf)

  def map[U](f: T => U): EventStream[U] = {
    new ChildEventSource[U, Unit] {
      override def debugName = "%s.map(%s)" format (EventSource.this.debugName, f)
      val f0 = f
      def handler = (event, _) => this fire f(event)
    }
  }

  def foreach(f: T => Unit)(implicit observing: Observing): Unit = {
    observing.addRef(f)
    observing.addRef(this)
    addListener(f)
    trace(AddedForeachListener(f))
    trace(HasListeners(listeners))
  }

  def filter(f: T => Boolean): EventStream[T] = new ChildEventSource[T, Unit] {
    override def debugName = "%s.filter(%s)" format (EventSource.this.debugName, f)
    val f0 = f
    def handler = (event, _) => if (f(event)) fire(event)
  }

  def takeWhile(p: T => Boolean): EventStream[T] = new ChildEventSource[T, Unit] {
    override def debugName = EventSource.this.debugName+".takeWhile("+p+")"
    def handler = (event, _) =>
      if (p(event))
        fire(event)
      else
        EventSource.this.removeListener(listener)
  }

  def foldLeft[U](initial: U)(f: (U, T) => U): EventStream[U] = new FoldedLeft(initial, f)

  def nonrecursive: EventStream[T] = new ChildEventSource[T, Unit] {
    override def debugName = "%s.nonrecursive" format (EventSource.this.debugName)
    protected val firing = new scala.util.DynamicVariable(false)
    def handler = (event, _) => if (!firing.value) firing.withValue(true) {
      fire(event)
    }
  }

  def distinct: EventStream[T] = {
    val folded = new FoldedLeft[List[T]](Nil, {
      case (Nil, e)      => e :: Nil
      case (old :: _, e) => e :: old :: Nil
    })
    new folded.Collected({
      case e :: Nil                  => e
      case e :: old :: _ if e != old => e
    }) {
      override def debugName = EventSource.this.debugName+".distinct"
    }
  }

  def |[U >: T](that: EventStream[U]): EventStream[U] = new EventSource[U] {
    override def debugName = "("+EventSource.this.debugName+" | "+that.debugName+")"
    val parent = EventSource.this
    val f: U => Unit = fire _

    EventSource.this addListener f
    that addListener f
  }

  def hold[U >: T](init: U): Signal[U] = new Signal[U] {
    override def debugName = EventSource.this.debugName+".hold("+init+")"
    private lazy val initial: U = init
    private var current: U = init
    def now = current

    val change = EventSource.this

    val f = (v: T) => current = v
    change addListener f
  }

  def zipWithStaleness: EventStream[(T, () => Boolean)] = new ChildEventSource[WithVolatility[T], Option[Volatility]](None) {
    override def debugName = "%s.withStaleness" format EventSource.this.debugName
    def handler = {
      case (parentEvent, volatilityOption) =>
        volatilityOption.foreach(_.stale = true)
        val volatility = new Volatility
        fire((parentEvent, volatility))
        Some(volatility)
    }
  }

  def throttle(period: Long): EventStream[T] = new Throttled(period)

  def nonblocking: EventStream[T] = new ActorEventStream

  private[reactive] def addListener(f: (T) => Unit): Unit = synchronized {
    trace(AddingListener(f))
    listeners :+= new WeakReference(f)
  }
  private[reactive] def removeListener(f: (T) => Unit): Unit = synchronized {
    //remove the last listener that is identical to f
    listeners.lastIndexWhere(_.get.map(f.eq) getOrElse false) match {
      case -1 =>
      case n =>
        listeners = listeners.patch(n, Nil, 1)
    }
  }
}

/**
 * This trait adds the ability to an event stream
 * to fire an event when the first listener is
 * added.
 * @author nafg
 *
 */
trait TracksAlive[T] extends EventSource[T] {
  private val aliveVar = Var(false)
  /**
   * This signal indicates whether the event stream
   * is being listened to
   */
  val alive: Signal[Boolean] = aliveVar.map{ x => x } // read only
  override def foreach(f: T => Unit)(implicit observing: Observing) {
    if (!aliveVar.now) {
      aliveVar() = true
    }
    super.foreach(f)(observing)
  }
}

/**
 * This EventStream allows one to block events
 * from within a certain scope. This can be used
 * to help prevent infinite loops when two EventStreams may depend on each other.
 */
//TODO suppressable event streams' transformed derivatives
//should also be Suppressable
trait Suppressable[T] extends EventSource[T] {
  protected val suppressed = new DynamicVariable(false)
  /**
   * Runs code while suppressing events from being fired on the same thread.
   * While running the code, calls to 'fire' on the same thread do nothing.
   * @param p the code to run while suppressing events
   * @return the result of evaluating p
   */
  def suppressing[R](p: => R): R = suppressed.withValue(true)(p)
  override def fire(event: T) = if (!suppressed.value) super.fire(event)
}

/**
 * This EventStream fires SeqDeltas (Seq deltas) and can batch them up.
 */
//TODO batchable event streams' transformed derivatives
//should also be Batchable
trait Batchable[A, B] extends EventSource[SeqDelta[A, B]] {
  protected val batch = new DynamicVariable(List[SeqDelta[A, B]]())
  private val inBatch = new DynamicVariable(false)
  /**
   * Runs code while batching messages.
   * While the code is running, calls to 'fire' on the same
   * thread will not fire messages immediately, but will collect
   * them. When the code completes, the messages are wrapped in a
   * single Batch which is then fired. If there is only one message
   * to be fired it is not wrapped in a Batch but fired directly.
   * Nested calls to batching are ignored, so all messages
   * collected from within the outermost call are collected and
   * they are fired in one batch at the end.
   * @param p the code to run
   * @return the result of evaluating p
   */
  def batching[R](p: => R): R = if (batch.value.isEmpty) {
    val ret = inBatch.withValue(true) { p }
    batch.value match {
      case Nil =>
      case msg :: Nil =>
        super.fire(msg)
      case msgs =>
        super.fire(Batch(msgs.reverse: _*))
    }
    batch.value = Nil
    ret
  } else {
    p
  }
  override def fire(msg: SeqDelta[A, B]) = {
    if (inBatch.value)
      batch.value ::= msg
    else
      super.fire(msg)
  }
}

/**
 * An EventStream that is implemented by delegating everything to another EventStream
 */
trait EventStreamProxy[T] extends EventStream[T] {
  protected[this] def self: EventStream[T]

  def debugString = self.debugString
  def debugName = self.debugName
  def flatMap[U](f: T => EventStream[U]): EventStream[U] = self.flatMap[U](f)
  def foldLeft[U](z: U)(f: (U, T) => U): EventStream[U] = self.foldLeft[U](z)(f)
  def map[U](f: T => U): EventStream[U] = self.map[U](f)
  def foreach(f: T => Unit)(implicit observing: Observing): Unit = self.foreach(f)(observing)
  def |[U >: T](that: EventStream[U]): EventStream[U] = self.|(that)
  def filter(f: T => Boolean): EventStream[T] = self.filter(f)
  def collect[U](pf: PartialFunction[T, U]): EventStream[U] = self.collect(pf)
  def takeWhile(p: T => Boolean): EventStream[T] = self.takeWhile(p)
  def hold[U >: T](init: U): Signal[U] = self.hold(init)
  def nonrecursive: EventStream[T] = self.nonrecursive
  def distinct: EventStream[T] = self.distinct
  def nonblocking: EventStream[T] = self.nonblocking
  def zipWithStaleness: EventStream[(T, () => Boolean)] = self.zipWithStaleness
  def throttle(period: Long): EventStream[T] = self.throttle(period)
  private[reactive] def addListener(f: (T) => Unit): Unit = self.addListener(f)
  private[reactive] def removeListener(f: (T) => Unit): Unit = self.removeListener(f)

}
