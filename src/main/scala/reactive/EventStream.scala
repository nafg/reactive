package reactive

import scala.ref.WeakReference

trait Observing {
  implicit val _: Observing = this
  private[reactive] var refs = List[AnyRef]()
  def observe[T](s: Signal[T])(f: T => Unit) = s.change foreach f
  def on[T](e: EventStream[T])(f: T => Unit) = e foreach f
  
}

trait EventStream[T] { parent =>
  class FlatMapped[U](initial: Option[T])(f:T=>EventStream[U])(implicit observing: Observing) extends EventStream[U] {
    // thread-unsafe implementation for now
    val thunk = (u: U) => fire(u)
    var curES: Option[EventStream[U]] = initial.map(f)
    curES.foreach{es =>
      es.listeners = new WeakReference(thunk) :: es.listeners.filter(_.get ne None)
    }
    for(parentEvent <- parent) {
      curES.foreach{es =>
        es.listeners = es.listeners.filter{ _.get match {
          case Some(t) => thunk ne t
          case None => false
        }}
      }
      curES = Some(f(parentEvent))
      curES.foreach{es =>
        es.listeners = new WeakReference(thunk) :: es.listeners.filter(_.get ne None)
      }
    }
  }
  private var listeners: List[WeakReference[T => Unit]] = Nil
  
  def hasListeners = !listeners.isEmpty
  
  def fire(event: T) {
//    val notCollected = listeners.count(_.get ne None)
//    println("EventStream " + (this) + " firing " + event +
//        " to " + listeners.size + " listeners of which " + notCollected + " are not gc'd")
//    println(listeners.map(_.get.map(_.getClass)).mkString("[",",","]"))
    listeners.foreach{_.get.foreach(_(event))}
//    println
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
    
  def flatMap[U](initial: T)(f: T=>EventStream[U])(implicit observing: Observing): EventStream[U] =
    new FlatMapped(Some(initial))(f)(observing)
  
  /**
   * Returns a new EventStream that, for every event t fired by
   * the original EventStream, fires an event (u,t) of type (U, T),
   * where u is the value calculated from the previous event.
   */
  def foldLeft[U](z: U)(f: (U,T)=>U)(implicit observing: Observing): EventStream[(U,T)] = new EventStream[(U,T)] {
    // may not be thread safe
    private var lastU = z
    for(t <- EventStream.this) {
      fire((lastU, t))
      lastU = f(lastU, t)
    }
  }
  
  def map[U](f: T=>U)(implicit observing: Observing): EventStream[U] = {
    val ret = new EventStream[U] {}
    foreach{event => ret fire f(event)}
    ret
  }
  
  def foreach(f: T=>Unit)(implicit observing: Observing): Unit = {
    listeners = new WeakReference(f) :: listeners.filter(_.get ne None)
    observing.refs ::= f
  }
  
  def filter(f: T=>Boolean)(implicit observing: Observing): EventStream[T] = new EventStream[T] {
    for(event <- EventStream.this) {
      if(f(event)) fire(event)
    }
  }
  
  def |(that: EventStream[T])(implicit observing: Observing): EventStream[T] = {
    val ret = new EventStream[T] {}
    this.foreach(ret.fire)
    that.foreach(ret.fire)
    ret
  }
}

trait EventStreamProxy[T] extends EventStream[T] {
  def underlying: EventStream[T]
  override def fire(event: T) = underlying.fire(event)
  override def flatMap[U](f: T=>EventStream[U])(implicit observing: Observing): EventStream[U] = underlying.flatMap[U](f)(observing)
  override def foldLeft[U](z: U)(f: (U,T)=>U)(implicit observing: Observing): EventStream[(U,T)] = underlying.foldLeft[U](z)(f)(observing)
  override def map[U](f: T=>U)(implicit observing: Observing): EventStream[U] = underlying.map[U](f)(observing)
  override def foreach(f: T=>Unit)(implicit observing: Observing): Unit = underlying.foreach(f)(observing)
  override def |(that: EventStream[T])(implicit observing: Observing): EventStream[T] = underlying.|(that)(observing)
}
