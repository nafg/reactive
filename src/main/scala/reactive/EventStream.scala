package reactive

import scala.ref.WeakReference
import scala.util.DynamicVariable

trait Observing {
  implicit val observing: Observing = this
  private var refs = List[AnyRef]()
  private[reactive] def addRef(ref: AnyRef) { refs ::= ref }
  def observe[T](s: Signal[T])(f: T => Unit) = s.change foreach f
  def on[T](e: EventStream[T])(f: T => Unit) = e foreach f
}
trait ObservingGroup extends Observing {
  protected def observings: List[Observing]
  override private[reactive] def addRef(ref: AnyRef) = observings foreach {_.addRef(ref)}
}

trait EventStreamBase[+T] {
  def foreach(f: T=>Unit)(implicit observing: Observing): Unit
}
trait EventStream[T] extends EventStreamBase[T] { parent =>
  var debug = false
  class FlatMapped[U](initial: Option[T])(f: T=>EventStream[U])(implicit observing: Observing) extends EventStream[U] {
    // thread-unsafe implementation for now
    val thunk: U=>Unit = fire _ // = (u: U) => fire(u)
    var curES: Option[EventStream[U]] = initial.map(f)
    curES.foreach{es =>
      es.listeners = es.listeners.filter(_.get.isDefined) ++ List(new WeakReference(thunk))
    }
    for(parentEvent <- parent) {
      curES.foreach{es =>
        es.listeners = es.listeners.filter{ _.get.map(thunk ne).getOrElse(false)}
      }
      curES = Some(f(parentEvent))
      curES.foreach{es =>
        es.listeners = es.listeners.filter(_.get.isDefined) ++ List(new WeakReference(thunk))
      }
    }
  }
  private var listeners: List[WeakReference[T => Unit]] = Nil
  
  def hasListeners = !listeners.isEmpty
  
  protected def dumpListeners {
    println(listeners.map(_.get.map(x => x.getClass + "@" + System.identityHashCode(x) + ": " + x.toString)).mkString("[",",","]"))
  }
  
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
   */
  def flatMap[U](f: T=>EventStream[U])(implicit observing: Observing): EventStream[U] =
    new FlatMapped(None)(f)(observing)
    
  //TODO this should become Signal#flatMap (which can be accessed from an EventStream via EventStream#Hold)
  def flatMap[U](initial: T)(f: T=>EventStream[U])(implicit observing: Observing): EventStream[U] =
    new FlatMapped(Some(initial))(f)(observing)
  
  /**
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
  
  def map[U](f: T=>U)(implicit observing: Observing): EventStream[U] = {
    new EventStream[U] {
      EventStream.this.foreach{event => this fire f(event)}
    }
  }
  
  def foreach(f: T=>Unit)(implicit observing: Observing): Unit = {
    listeners = listeners.filter(_.get.isDefined) ++ List(new WeakReference(f))
    if(debug) {
      println("Added a listener to " + this)
      dumpListeners
    }
    
    observing.addRef(f)
  }
  
  def filter(f: T=>Boolean)(implicit observing: Observing): EventStream[T] = new EventStream[T] {
    for(event <- EventStream.this) {
      if(f(event)) fire(event)
    }
  }
  
  def foldLeft[U](initial: U)(f: (U,T)=>U)(implicit observing: Observing): EventStream[U] = new EventStream[U] {
    var last = initial
    for(event <- EventStream.this) {
      last = f(last, event)
      fire(last)
    }
  }
  
  def |(that: EventStream[T])(implicit observing: Observing): EventStream[T] = {
    val ret = new EventStream[T] {}
    this.foreach(ret.fire)
    that.foreach(ret.fire)
    ret
  }
  
  def hold(initial0: =>T)(implicit observing: Observing): Signal[T] = new Signal[T] {
    private lazy val initial: T = initial0
    private var current: Option[T] = None
    def change = EventStream.this
    change foreach {v => current = Some(v)}
    def now = current getOrElse initial
  }
  
  def forward(recipient: EventStream[T])(implicit observing: Observing) {
    this foreach recipient.fire
  }
}

/**
 * This trait adds the ability to an event stream
 * to fire an event when the first listener is
 * added. 
 * @author nafg
 *
 */
trait TracksAlive[T] extends EventStream[T] {
  /**
   * This signal indicates whether the event stream
   * is being listened to 
   */
  private val aliveVar = Var(false)
  private val o = new Observing {}
  val alive: Signal[Boolean] = aliveVar.map(identity)(o) // read only
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
  to help prevent infinite loops.
*/
trait Suppressable[T] extends EventStream[T] {
  protected val suppressed = new DynamicVariable(false)
  def suppressing[R](p: =>R) = suppressed.withValue(true)(p)
  override def fire(event: T) = if(!suppressed.value) super.fire(event)
}
/**
  This EventStream fires Messages (Seq deltas) and can batch them up.
*/
trait Batchable[A,B] extends EventStream[Message[A,B]] {
  protected val batch = new DynamicVariable(List[Message[A,B]]())
  private val inBatch = new DynamicVariable(false)
  def batching[R](p: =>R) = if(batch.value.isEmpty) {
    inBatch.withValue(true) {p}
    batch.value match {
      case Nil =>
      case msgs =>
        super.fire(Batch(msgs.reverse: _*))
    }
  }
  override def fire(msg: Message[A,B]) = {
    if(inBatch.value)
      batch.value ::= msg
    else
      super.fire(msg)
  }
}

trait EventStreamProxy[T] extends EventStream[T] {
  def underlying: EventStream[T]
  override def fire(event: T) = underlying.fire(event)
  override def flatMap[U](f: T=>EventStream[U])(implicit observing: Observing): EventStream[U] = underlying.flatMap[U](f)(observing)
  //override def foldLeft[U](z: U)(f: (U,T)=>U)(implicit observing: Observing): EventStream[(U,T)] = underlying.foldLeft[U](z)(f)(observing)
  override def foldLeft[U](z: U)(f: (U,T)=>U)(implicit observing: Observing): EventStream[U] = underlying.foldLeft[U](z)(f)(observing)
  override def map[U](f: T=>U)(implicit observing: Observing): EventStream[U] = underlying.map[U](f)(observing)
  override def foreach(f: T=>Unit)(implicit observing: Observing): Unit = underlying.foreach(f)(observing)
  override def |(that: EventStream[T])(implicit observing: Observing): EventStream[T] = underlying.|(that)(observing)
}
