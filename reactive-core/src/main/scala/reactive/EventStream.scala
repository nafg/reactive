package reactive

import scala.util.DynamicVariable
import scala.collection.mutable.WeakHashMap

object EventStream {
  object empty extends EventSource[Nothing]
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
trait EventStream[+T] extends Forwardable[T] {
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
   * This is ideal for handling events in ways that are time consuming.
   * Subsequent events are handled sequentially.
   * The signal will hold values of type (T, ()=>Boolean), where T is the type of the
   * parent event stream, and the value tuple will contain the event fired in the parent
   * as well as a function that can be used to test
   * whether that event is outdated because a new event has been fired since.
   * This is because the sequential nature means that a new event cannot
   * be received until the previous event is finished being handled.
   * The test function is useful because it may be desirable to abort the time-consuming work
   * if the event has been superseded
   * Example usage:
   * for((v, isSuperseded) <- eventStream.nonblocking) { doSomework(); if(!isSuperseded()) doSomeMoreWork() }
   * If you don't care whether it was superseded just do
   * for((v, _) <- eventStream.nonblocking) ...
   */
  def nonblocking: EventStream[(T, () => Boolean)]

  private[reactive] def addListener(f: (T) => Unit): Unit
  private[reactive] def removeListener(f: (T) => Unit): Unit

  def debugString: String
}

class NamedFunction[-T, +R](name: => String, f: T => R) extends (T => R) {
  def apply(in: T): R = f(in)
  override def toString = "%s #%s" format (name, System.identityHashCode(f))
}
object NamedFunction {
  def apply[T, R](name: => String)(f: T => R) = new NamedFunction(name, f)
}

case class ScopedFunction[-T, +R](val function: T => R) extends (T => R) {
  def newGroup = {
    val parent = EventSource.currentListenerScope.value
    val ret = new EventSource.ListenerScope(Some(parent), this)
    parent.synchronized { parent.subgroups :+= ret }
    ret
  }

  var group = newGroup
  def apply(e: T): R = {
    //println("Invoking "+toString)
    synchronized {
      group.clear
      group = newGroup
    }
    val ret = EventSource.currentListenerScope.withValue(group) {
      function(e)
    }
    //println("Finished invoking "+toString)
    ret
  }
  override def toString = "ScopedFunction(%s: %s #%s)" format (function.toString, function.getClass, System.identityHashCode(function))
}

object EventSource {
  @deprecated("Use Logger.defaultLevel, this does nothing anymore")
  var debug = false
  /*
   * We need to store globally nested listener groups.
   * Deleting a group removes all listeners inside it from all EventSources.
   * Ability to get all listeners for a given EventSource
   * 
   * Scenarios where you would want listeners to remain:
   *  • Clicking a button adds a new item, which has its own listeners.
   *    The next time you click the button, the item should not lose its listeners!
   *    We can differentiate by using forwarding instead of foreach
   *  • A foreach inside a forwarding block
   *    sigA =>> { _ => for(x <- sigX) doSomething(x) } // keep adding more listeners to sigX
   *
   * Scenarios where you would want listeners to be replaced:
   *  • A foreach inside a foreach
   *    for(a <- sigA; b <- sigB) doSomething(a, b)
   *  • A map inside a flatMap
   *    for(a <- sigA; b <- sigB) yield getSomethingWithASideEffect(a, b)
   *  • A forwarding block inside a foreach 
   *    for(_ <- sigA) sigX =>> doSomething             // replace listener in sigX
   *    
   * Questionable scenarios -- wrap the function in ScopedFunction to explicitly replace listeners
   *  • A map inside a map 
   *    sigA map ScopedFunction { a => Cell(for(b <- sigB) yield b) }          // re-rendering the (new) Cell causes old Cell's listener to be removed from sigB-mapped
   *  • sigA map { a => val i = new Item(a); i.button ->> i.doSomething(); i}  // not scoped, so listener remains
   *  
   */

  class ListenerScope(val parent: Option[ListenerScope], sf: ScopedFunction[_, _]) {
    val _listeners = new WeakHashMap[EventSource[_], WeakBuffer[_ => Unit]]()
    var subgroups = List[ListenerScope]()

    def clear {
      if (_listeners.nonEmpty || (subgroups.nonEmpty && subgroups.exists(_._listeners.nonEmpty))) {
        //        println("Clearing listeners:")
        //        printTree()
        _listeners.synchronized {
          _listeners.clear()
        }
      }
      subgroups = Nil
      parent foreach (p => p.subgroups = p.subgroups filterNot (this eq))
    }

    def listeners[T](es: EventSource[T]): Iterator[T => Unit] =
      _listeners.getOrElse(es, Seq.empty).iterator.map(_.asInstanceOf[T => Unit]) ++ subgroups.iterator.flatMap(_.listeners(es))

    def addListener[T](es: EventSource[T], listener: T => Unit) = _listeners.synchronized {
      val ls = _listeners.getOrElseUpdate(es, new WeakBuffer)
      ls += listener
      ls.compact(0)
    }

    def removeListener[T](es: EventSource[T], listener: T => Unit): Boolean = _listeners.synchronized {
      val ls = _listeners.getOrElseUpdate(es, new WeakBuffer)
      val last = ls.lastIndexWhere{
        case ScopedFunction(f) => f eq listener
        case f                 => f eq listener
      }
      last match {
        case -1 =>
          subgroups.exists(_.removeListener(es, listener))
        case n =>
          ls.remove(n)
          true
      }
    }

    def printTree(indent: Int = 0) {
      _listeners.synchronized {
        println((" " * indent) + toString + _listeners.map{ case (es, ls) => (es.debugName, ls) }.mkString("[", ",", "]"))
      }
      subgroups foreach (_.printTree(indent + 2))
    }
    override def toString = "ListenerScope for "+sf.toString
  }

  private[reactive] object rootListenerScope extends ListenerScope(None, null) {
    override def toString = "rootListenerScope"
  }
  private[reactive] val currentListenerScope = new DynamicVariable[ListenerScope](rootListenerScope)
}

/**
 * A basic implementation of EventStream,
 * adds fire method.
 */
//TODO perhaps EventSource = SimpleEventStream + fire
class EventSource[T] extends EventStream[T] with Logger {
  case class HasListeners(listeners: List[T => Unit]) extends LogEventPredicate
  case class FiringEvent(event: T, listenersCount: Int) extends LogEventPredicate
  case class AddingListener(listener: T => Unit) extends LogEventPredicate
  case class AddedForeachListener(listener: T => Unit) extends LogEventPredicate

  @deprecated("Use logLevel or setLogLevel, this does nothing anymore")
  var debug = EventSource.debug

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
    override def debugName = "%s.flatMap(%s)" format (EventSource.this, f)
    val thunk: U => Unit = fire _
    state foreach { _ addListener thunk }
    def handler = (parentEvent, lastES) => {
      lastES foreach { _ removeListener thunk }
      val newES = Some(f(parentEvent))
      newES foreach { _ addListener thunk }
      newES
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

  class ActorEventStream extends ChildEventSource[WithVolatility[T], Option[Volatility]](None) {
    override def debugName = "%s.nonblocking" format (EventSource.this.debugName)
    import scala.actors.Actor._
    private val delegate = actor {
      loop {
        receive {
          case (x: T, volatility: Volatility) => fire((x, volatility))
        }
      }
    }
    def handler = {
      case (parentEvent, volatilityOption) =>
        volatilityOption.foreach(_.stale = true)
        val volatility = new Volatility
        delegate ! (parentEvent, volatility)
        Some(volatility)
    }
  }

  def allListeners = EventSource.rootListenerScope.listeners(this)

  /**
   * Whether this EventStream has any listeners depending on it
   */
  //TODO should it return false if it has listeners that have been gc'ed?
  def hasListeners = allListeners.nonEmpty // _listeners.values.nonEmpty && _listeners.values.forall(_.nonEmpty /*forall(_.get.isDefined)*/ )

  def debugName = "(eventSource: %s #%s)".format(getClass, System.identityHashCode(this))
  def debugString = {
    "%s\n  listeners%s".format(
      debugName,
      allListeners.mkString("\n    ", "\n    ", "\n")
    )
  }

  /**
   * Sends an event to all listeners.
   * @param event the event to send
   */
  def fire(event: T) {
    trace(HasListeners(allListeners.toList))
    trace(FiringEvent(event, allListeners.size))
    allListeners foreach (_(event))
  }

  def flatMap[U](f: T => EventStream[U]): EventStream[U] =
    new FlatMapped(None)(f)

  //TODO should this become Signal#flatMap (which can of course be accessed from an EventStream via EventStream#Hold) or be renamed?
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

  /**
   * Registers a listener function to run whenever
   * an event is fired. The function is help with a WeakReference,
   * and a strong reference
   * is placed in the Observing, so the latter determines
   * the function's gc lifetime.
   * NOTE: All listeners which are added to any EventSource
   * during executing of f, will be removed every time
   * f is executed subsequently! If this is not the
   * behavior you want, use forward or one of the forwarding operators.
   * @param f a function to be applied on every event
   * @param observing the object whose gc lifetime should determine that of the function
   */
  def foreach(f: T => Unit)(implicit observing: Observing): Unit = {
    val sl = ScopedFunction(f)
    forward(sl)
    trace(AddedForeachListener(sl))
  }

  /**
   * Like foreach but listeners added to EventSources during
   * f's executing will not be "scoped" until the next time
   * f is invoked.
   */
  def forward(f: T => Unit)(implicit observing: Observing) {
    observing.addRef(f)
    observing.addRef(this)
    addListener(f)
    trace(HasListeners(allListeners.toList))
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
      else {
        EventSource.this.removeListener(listener)
      }
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
    val parent = EventSource.this
    val f: U => Unit = fire _

    EventSource.this addListener f
    that addListener f
  }

  def hold[U >: T](init: U): Signal[U] = new Signal[U] {
    private lazy val initial: U = init
    private var current: U = init
    def now = current

    val change = EventSource.this

    val f = (v: T) => current = v
    change addListener f
  }

  def nonblocking: EventStream[(T, () => Boolean)] = new ActorEventStream

  private[reactive] def addListener(f: T => Unit): Unit =
    EventSource.currentListenerScope.value.addListener(this, f)

  private[reactive] def removeListener(f: T => Unit): Unit =
    EventSource.rootListenerScope.removeListener(this, f)
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
trait EventSourceProxy[T] extends EventSource[T] {
  def underlying: EventSource[T]
  override def fire(event: T) = underlying.fire(event)
  override def flatMap[U](f: T => EventStream[U]): EventStream[U] = underlying.flatMap[U](f)
  override def foldLeft[U](z: U)(f: (U, T) => U): EventStream[U] = underlying.foldLeft[U](z)(f)
  override def map[U](f: T => U): EventStream[U] = underlying.map[U](f)
  override def foreach(f: T => Unit)(implicit observing: Observing): Unit = underlying.foreach(f)(observing)
  override def |[U >: T](that: EventStream[U]): EventStream[U] = underlying.|(that)
}
