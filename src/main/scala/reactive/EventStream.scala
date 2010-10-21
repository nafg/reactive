package reactive

import scala.ref.WeakReference
import scala.util.DynamicVariable


/**
 * Keeps a list of strong references. Used to control when observers
 * can be garbage collected. The observable uses weak references to hold
 * the observers, so that observers aren't retained in memory for the
 * entire lifetime of the observable.
 * Therefore, to make sure the observer isn't garbage collected too early, a
 * reference to it is stored in an Observing.
 * Usage: Most methods that add observers to an observable take an Observing
 * as an implicit parameter, so usually you put an implicit Observing in
 * the scope in question, and make sure it lasts as long as you need the
 * observers to last. You can do this by having the containing class
 * extends Observing (it contains an implicit pointing to itself), or
 * by writing
 * implicit val observing = new Observing {}
 * or the like. You can also pass an Observing instance explicitly to
 * any method that takes one, bypassing the implicit resolution mechanism.
 */
trait Observing {
  /**
   * Places an implicit reference to 'this' in scope
   */
  implicit val observing: Observing = this
  private var refs = List[AnyRef]()
  private[reactive] def addRef(ref: AnyRef) { refs ::= ref }
  private[reactive] def removeRef(ref: AnyRef) {
    refs = refs.span(ref.ne) match {
      case (nes, firstEqs) => nes ++ firstEqs.drop(1)
    }
  }
  /**
   * You can write
   * [observing.] observe(signal){value => action}
   * as an alternative syntax to
   * signal.change.foreach{value => action} [(observing)]
   */
  def observe[T](s: Signal[T])(f: T => Unit) = s.change.foreach(f)(this)
  /**
   * You can write
   * [observing.] on(eventStream){event => action}
   * as an alternative syntax to
   * eventStream.change.foreach{event => action} [(observing)]
   */
  def on[T](e: EventSource[T])(f: T => Unit) = e.foreach(f)(this)
}

/**
 * An Observing that, rather than maintaing references itself,
 * maintains a List of Observings that all maintain all references.
 */
trait ObservingGroup extends Observing {
  protected def observings: List[Observing]
  override private[reactive] def addRef(ref: AnyRef) = observings foreach {_.addRef(ref)}
}



trait EventStream[+T] {
  def foreach(f: T=>Unit)(implicit observing: Observing): Unit
  def map[U](f: T=>U): EventStream[U]
  def flatMap[U](f: T=>EventStream[U]): EventStream[U]
  def filter(f: T=>Boolean): EventStream[T]
  def takeWhile(p: T=>Boolean): EventStream[T]
  def foldLeft[U](initial: U)(f: (U,T)=>U): EventStream[U]
  def |[U>:T](that: EventStream[U]): EventStream[U]
  def hold[U>:T](init: =>U): Signal[U]
  
  private[reactive] def addListener(f: (T) => Unit): Unit
  private[reactive] def removeListener(f: (T) => Unit): Unit
}
/**
 * An EventStream is a source of events (arbitrary values sent to listener functions).
 * You can fire events from it, you can react to events with any behavior, and you can
 * create derived EventStreams, whose events are based on the original EventStream.
 * The API is modeled after the Scala standard library collections framework.
 * An EventStream is like a collection in the sense that it consists of multiple values.
 * However, unlike actual collections, the values are not available upon request; they
 * occur whenever they occur. Nevertheless, many operations that apply to collections
 * apply to event streams. To react to events, use foreach or foldLeft. To create derived,
 * transformed EventStreams, use map, flatMap, filter, foldLeft, and the | (union) operator.
 * Note that you can of course use for comprehensions as syntactic sugar for many
 * of the above.
 * Methods that return a new EventStream generally do not require an (implicit) Observing.
 * Instead, the new EventStream itself holds references to the parent EventStream and
 * the event function (which refers to, or is, the function passed in to the method).
 * (As a result, if you derive EventStreams with a function that performs side effects,
 * in order to ensure that the function is not garbage collected you must retain a reference
 * to the resulting EventStream.)
 * On the other hand, methods which do require an Observing take functions which are expected
 * perform side effects, and therefore do not hold a reference to the function themselves but
 * rather use the Observing for that purpose. As a result, they will remain in memory as long
 * as the Observing object does. 
 * You can also create a Signal from an EventStream using hold.
 * @tparam T the type of values fired as events
 */
trait EventSource[T] extends EventStream[T] {
  var debug = false
  class FlatMapped[U](initial: Option[T])(val f: T=>EventStream[U]) extends EventSource[U] {
    // thread-unsafe implementation for now
    val thunk: U=>Unit = fire _ // = (u: U) => fire(u)
//    val thunkA: Any=>Unit = {case u: U => fire(u)}
    var curES: Option[EventStream[U]] = initial.map(f)
    curES.foreach{es =>
      es.addListener(thunk)
    }
    val parent = EventSource.this
    parent.addListener {parentEvent =>
      curES.foreach{es =>
        es.removeListener(thunk)
      }
      curES = Some(f(parentEvent))
      curES.foreach{es =>
        es.addListener(thunk)
      }
    }
  }
  
//  case class Listeners[-T](listeners: Seq[WeakReference[T => Unit]])
  
//  private var listeners = Listeners(Nil)
//  def fire[T1 >: T](event: T1) = listeners.listeners.foreach(_.get.foreach(_(event)))
  
  private var listeners: List[WeakReference[T => Unit]] = Nil
//  def listeners = l2.l.toList
//  def listeners_=(xs: List[WeakReference[(_ <: T) =>Unit]]) = l2 = Listeners(xs)
  
  /**
   * Whether this EventStream has any listeners depending on it
   */
  //TODO should it return false if it has listeners that have been gc'ed?
  def hasListeners = !listeners.isEmpty
  
  protected def dumpListeners {
    println(listeners.map(_.get.map(x => x.getClass + "@" + System.identityHashCode(x) + ": " + x.toString)).mkString("[",",","]"))
  }
  
  /**
   * Sends an event to all listeners.
   * @param event the event to send
   */
  //TODO should this really be public API in the main trait?
  def fire(event: T) {
    if(debug) {
      val notCollected = listeners.count(_.get ne None)
      println("EventStream " + (this) + " firing " + event +
          " to " + listeners.size + " listeners (of which " + notCollected + " are not gc'd)")
      dumpListeners
    }
    listeners.foreach{_.get.foreach(_(event))}
    if(debug) println
  }
  
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
  def flatMap[U](f: T=>EventStream[U]): EventStream[U] =
    new FlatMapped(None)(f)
    
  //TODO this should become Signal#flatMap (which can of course be accessed from an EventStream via EventStream#Hold)
  def flatMap[U](initial: T)(f: T=>EventStream[U]): EventStream[U] =
    new FlatMapped(Some(initial))(f)
  
  /* *
   * Returns a new EventStream that, for every event t fired by
   * the original EventStream, fires an event (u,t) of type (U, T),
   * where u is the value calculated from the previous event.
   */
 /*  def foldLeft[U](z: U)(f: (U,T)=>U)(implicit observing: Observing): EventStream[(U,T)] = new EventStream[(U,T)] {
    // may not be thread safe
    private var lastU = z
    for(t <- EventStream.this) {
      fire((lastU, t))
      lastU = f(lastU, t)
    }
  } */
  
  /**
   * Returns a new EventStream, that for every event that this EventStream
   * fires, that one will fire an event that is the result of
   * applying 'f' to this EventStream's event.
   * @param f the function that transforms events fired by this EventStream
   * into events to be fired by the resulting EventStream.
   */
  def map[U](f: T=>U): EventStream[U] = {
    new EventSource[U] {
      val parent = EventSource.this
      val f0 = f
      parent.addListener{event => this fire f(event)}
    }
  }
  
  /**
   * Adds a listener to this EventStream.
   * @param f a function to be applied on every event
   */
  def foreach(f: T=>Unit)(implicit observing: Observing): Unit = {
    addListener(f)
    if(debug) {
      println("Added a listener to " + this)
      dumpListeners
    }
    
    observing.addRef(f)
    observing.addRef(this)
  }
  
  /**
   * Returns a new EventStream that propagates a subset of the events that
   * this EventStream fires.
   * @param f the predicate function that determines which events will
   * be fired by the new EventStream.
   */
  def filter(f: T=>Boolean): EventStream[T] = new EventSource[T] {
    val parent = EventSource.this
    val f0 = f
    parent.addListener{event=>
      if(f(event)) fire(event)
    }
  }
  
  /**
   * Returns a new EventStream that propagates this EventStream's events
   * until the predicate returns false.
   * @param p the precate function, taking an event as its argument
   * and returning true if event propagation should continue
   */
  def takeWhile(p: T=>Boolean): EventStream[T] = new EventSource[T] {
    val parent = EventSource.this
    val f: T=>Unit = (event: T)=> {
      if(p(event)) fire(event) else {
//        observing.removeRef(f)
//        observing.removeRef(this)
//        println("Before removeListener: ")
//        dumpListeners
        EventSource.this.removeListener(f)
//        println("After removeListener: ")
//        dumpListeners
      }
    }
//    observing.addRef(f)
//    observing.addRef(this)
    parent.addListener(f)
  }
  
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
  def foldLeft[U](initial: U)(f: (U,T)=>U): EventStream[U] = new EventSource[U] {
    var last = initial
    val f0 = f
    val parent = EventSource.this
    parent.addListener{event=>
      last = f(last, event)
      fire(last)
    }
  }
  
  /**
   * Union of two EventStreams.
   * Returns a new EventStream that consists of all events
   * fired by both this EventStream and 'that.'
   * @param that the other EventStream to combine in the resulting
   * EventStream.
   */
  def |[U>:T](that: EventStream[U]): EventStream[U] = {
    val ret = new EventSource[U] {
      val parent = EventSource.this
      val f0: U=>Unit = fire _
    }
    val f = ret.f0
    this.addListener(f)
    that.addListener(f)
    ret
  }
  
  /**
   * Returns a Signal whose value is initially the 'init' parameter,
   * and after every event fired by this EventStream, the value of
   * that event.
   * @param init the initial value of the signal
   */
  def hold[U>:T](init: =>U): Signal[U] = new Signal[U] {
    private lazy val initial: U = init
    private var current: Option[T] = None
    
    val f = (v: T) => current = Some(v)
    val change = EventSource.this
    change addListener f
    
    def now = current getOrElse initial
  }
  
  /**
   * Causes all events fired by this EventStream to be sent to
   * another EventStream as well.
   * @param recipient the EventStream to forward events to
   */
  def forward(recipient: EventSource[T])(implicit observing: Observing) {
    this foreach recipient.fire
  }
  
  private[reactive] def addListener(f: (T) => Unit): Unit = {
    listeners = listeners.filter(_.get.isDefined) :+ new WeakReference(f)
  }
  private[reactive] def removeListener(f: (T) => Unit): Unit = {
    //remove the last listener that is identical to f.
    //do this with a foldRight, passing around the
    //part of the list already processed, and a boolean
    //indicating whether to remove the next occurrence of f.
    listeners.foldRight((List[WeakReference[T=>Unit]](), true)){
      case (wr,(list, true)) => wr.get match {
        case Some(l) if l eq f => (list, false)
        case _ => (list :+ wr, true)
      }
      case (wr,(list, false)) =>
        (if(wr.get.isDefined) list :+ wr else list, false)
    } match {
      case (l, _) => listeners = l
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
  //val alive: Signal[Boolean] = aliveVar.map[Boolean, Signal[Boolean]](identity) // read only
  override def foreach(f: T=>Unit)(implicit observing: Observing) {
    if(!aliveVar.now) {
      aliveVar()=true
    }
    super.foreach(f)(observing)
  }
}

/**
  This EventStream allows one to block events
  from within a certain scope. This can be used
  to help prevent infinite loops when two EventStreams may depend on each other.
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
  def suppressing[R](p: =>R): R = suppressed.withValue(true)(p)
  override def fire(event: T) = if(!suppressed.value) super.fire(event)
}

/**
  This EventStream fires Messages (Seq deltas) and can batch them up.
*/
//TODO batchable event streams' transformed derivatives
//should also be Batchable
trait Batchable[A,B] extends EventSource[Message[A,B]] {
  protected val batch = new DynamicVariable(List[Message[A,B]]())
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
  def batching[R](p: =>R): R = if(batch.value.isEmpty) {
    val ret = inBatch.withValue(true) {p}
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
  override def fire(msg: Message[A,B]) = {
    if(inBatch.value)
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
  override def flatMap[U](f: T=>EventStream[U]): EventStream[U] = underlying.flatMap[U](f)
  //override def foldLeft[U](z: U)(f: (U,T)=>U)(implicit observing: Observing): EventStream[(U,T)] = underlying.foldLeft[U](z)(f)(observing)
  override def foldLeft[U](z: U)(f: (U,T)=>U): EventStream[U] = underlying.foldLeft[U](z)(f)
  override def map[U](f: T=>U): EventStream[U] = underlying.map[U](f)
  override def foreach(f: T=>Unit)(implicit observing: Observing): Unit = underlying.foreach(f)(observing)
  override def |[U>:T](that: EventStream[U]): EventStream[U] = underlying.|(that)
}
