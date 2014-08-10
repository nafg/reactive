package reactive
package web
package javascript

import net.liftweb.json.{ Formats, DefaultFormats }

import JsTypes._

import scala.language.higherKinds

object JsEventStream {
  implicit def canForwardTo[T, J <: JsAny](implicit conv: ToJs.From[T]#To[J, JsExp]): CanForwardTo[JsEventStream[J], T] = new CanForwardTo[JsEventStream[J], T] {
    def forwarder(target: JsEventStream[J]) = v => target.fire(conv(v))
  }
}

/**
 * Proxies an event stream implemented in javascript.
 * Requires reactive-web.js (which is automatically included by the lift:reactive snippet).
 */
//TODO use PageIds??
//TODO use JsStub
class JsEventStream[T <: JsAny]()(implicit page: Page) extends JsExp[JsObj] with JsForwardable[T] { parent =>
  lazy val id = page.nextNumber
  private var initialized = false
  def initExp = "new EventStream()"
  def render = "reactive.eventStreams["+id+"]"
  def init: Unit = synchronized {
    if (!initialized) {
      initialized = true
      page queue render+"="+initExp
    }
  }
  private var ajaxQueued = false
  def queueAjax = synchronized {
    init
    if (!ajaxQueued) {
      ajaxQueued = true
      foreach(JsRaw[T =|> JsVoid]("reactive.queueAjax("+id+")"))
    }
  }

  protected def child[U <: JsAny](renderer: => String) = {
    init
    new JsEventStream[U]()(page) {
      override def initExp: String = renderer
    }
  }
  /**
   * The javascript event stream's fire method, as a JsExp[JsFunction1[T,Void]]
   */
  def fireExp: $[T =|> JsVoid] = {
    init
    JsRaw(render+".fire")
  }
  /**
   * Invoke the javascript event stream's fire method.
   * All ajax calls queued (such as by invoking fire)
   * are scheduled to be processed after 500 milliseconds.
   */
  def fire(v: JsExp[T]) {
    page queue JsExp.render(fireExp(v))
    page queue s"window.setTimeout(function() { reactive.doAjax('${page.id}') }, 500)"
  }

  protected[reactive] def foreachImpl(f: $[T =|> JsVoid]) {
    init
    page queue render+".foreach("+JsExp.render(f)+")"
  }
  /**
   * Register a javascript callback function with the javascript event stream.
   */
  def foreach[E[J <: JsAny] <: JsExp[J], F: ToJs.To[JsFunction1[T, JsVoid], E]#From](f: F) {
    foreachImpl(f)
  }
  /**
   * Register a javascript callback function with the javascript event stream.
   */
  def foreach(f: $[T =|> JsVoid]) {
    foreachImpl(f)
  }
  /**
   * Returns an EventStream that fires on the server the events fired by the javascript event stream
   * in the browser.
   * @tparam U the Scala type of the EventStream's values
   * @param extract a function that takes a value of type JValue (a lift-json AST) and returns values of type U
   */
  def toServer[U](extract: net.liftweb.json.JValue => U): EventStream[U] = {
    queueAjax
    page.ajaxEvents.collect { case (_id, json) if _id == id.toString => extract(json) }
  }
  /**
   * Returns an EventStream that fires on the server the events fired by the javascript event stream
   * in the browser, using lift-json's flexible implicits-based json extraction --
   * i.e., it calls toServer(_.extract(formats, manifest))
   * @tparam U the Scala type of the EventStream's values
   */
  def toServer[U](implicit formats: Formats = DefaultFormats, manifest: Manifest[U]): EventStream[U] =
    toServer(_.extract(formats, manifest))

  /**
   * Returns a new JsEventStream that proxies a new javascript event stream, derived
   * from the original javascript event stream with a mapping function.
   */
  def map[U <: JsAny, F: ToJs.To[JsFunction1[T, U], JsExp]#From](f: F): JsEventStream[U] = child(JsExp.render(parent)+".map("+JsExp.render(f)+")")
  //  def map[U<:JsAny](f: $[T=|>U]): JsEventStream[U] = child(parent.render+".map("+f.render+")")
  /**
   * Returns a new JsEventStream that proxies a new javascript event stream, derived
   * from the original javascript event stream with a flat-mapping function.
   */
  def flatMap[U <: JsAny, F <% JsExp[JsFunction1[T, U]]](f: F): JsEventStream[U] = child(JsExp.render(parent)+".flatMap("+JsExp.render(f)+")")
  /**
   * Returns a new JsEventStream that proxies a new javascript event stream, derived
   * from the original javascript event stream with a filtering function.
   */
  def filter[F <% JsExp[JsFunction1[T, JsBoolean]]](f: F): JsEventStream[T] = child(JsExp.render(parent)+".filter("+JsExp.render(f)+")")

  /**
   * Returns a JsEventStream proxying a new javascript event stream, derived from this one,
   * that only fires the events that are not followed by another event within ''period'' milliseconds.
   */
  def throttle(period: Long): JsEventStream[T] = child(JsExp.render(parent)+".throttle("+period.toString+")")

  //  def takeWhile(p: T=>Boolean): EventStream[T]
  //  def foldLeft[U](initial: U)(f: (U,T)=>U): EventStream[U]
  //  def |[U>:T](that: EventStream[U]): EventStream[U]
  //  def hold[U>:T](init: U): Signal[U]
  //  
  //  def nonrecursive: EventStream[T]

}
/**
 * Typeclass for types that values can be forwarded to
 * via javascript.
 */
trait CanForwardJs[-T, V <: JsAny] {
  def forward(s: JsForwardable[V], t: T)
}
object CanForwardJs {
  implicit def jes[V <: JsAny] = new CanForwardJs[JsEventStream[V], V] {
    def forward(s: JsForwardable[V], t: JsEventStream[V]) =
      s.foreach((x: $[V]) => t.fireExp(x))
  }
}

/**
 * The client-side complement to Forwardable.
 * Defines a convenience method for common foreach invocations.
 * Implementations of this trait produce values that can be forwarded in javascript
 * to a type that has a CanForwardJs typeclass available.
 */
trait JsForwardable[T <: JsAny] {
  def foreach[E[J <: JsAny] <: JsExp[J], F: ToJs.To[JsFunction1[T, JsVoid], E]#From](f: F)
  def foreach(f: $[T =|> JsVoid])

  /**
   * Forwards values to something whose type has an implicit CanForwardJs defined,
   * such as a JsEventStream, directly within the client.
   */
  def ~>>[S](target: => S)(implicit canForward: CanForwardJs[S, T]): this.type = {
    canForward.forward(this, target)
    this
  }
}


object JS extends App {
  import org.jscala._
  @Javascript(json = false)
  class EventStream[A] {
    val listeners = JArray[A => Unit]()
    def foreach(f: A => Unit) = {
      listeners.push(f)
    }
    def addListener(f: A => Unit) = foreach(f)
    def removeListener(f: A => Unit) = {
      def loop(i: Int): Unit = if(i < listeners.length) {
        if(listeners(i) == f) listeners.delete(i)
        else loop(i + 1)
      }
      loop(0)
    }
    val fire: A => Unit = { v =>
      forIn(listeners.as[Seq[A => Unit]])(listeners(_)(v))
    }
    def map[B](f: A => B) = {
      val mapped: EventStream[B] = new EventStream[B]()
      addListener(a => mapped.fire(f(a)))
      mapped
    }
    def flatMap[B](f: A => EventStream[B]) = {
      val flatMapped = new EventStream[B]
      var lastES: EventStream[B] = null
      addListener{v =>
        if(lastES != null) lastES.removeListener(flatMapped.fire)
        lastES = f(v)
        lastES.addListener(flatMapped.fire)
      }
      flatMapped
    }
    def filter(f: A => Boolean) = {
      val filtered = new EventStream[A]
      addListener(v => if(f(v)) filtered.fire(v))
      filtered
    }
    def throttle(period: Int) = {
      val throttled = new EventStream[A]
      var last: A = null.asInstanceOf[A]
      val onTimer = () => {
        if(last != null) throttled.fire(last) // TODO should be !== undefined
        last = null.asInstanceOf[A]
      }
      var to = window.setTimeout(onTimer, period)
      addListener{ v =>
        window.clearTimeout(to)
        last = v
        to = window.setTimeout(onTimer, period)
      }
      throttled
    }
  }
  object EventStream
  println(EventStream.javascript.asString)

  println(javascript {
    window.reactive = new {
      def error(e: String) = if(window.console.as[Boolean]) window.console.error(e)
      var queuedAjaxEvents = JArray[Any]()
      var unique = 0
      var eventStreams = collection.mutable.Map[Int, EventStream[Any]]()
      def fire(i: Int, value: Any) = {
        if(!eventStreams(i).as[Boolean])
          eventStreams(i) = new EventStream[Any]
        eventStreams(i).fire(value)
      }
      def queueAjax(id: Int) = { (value: Any) =>
        val e = collection.mutable.Map[Int, Any]()
        e(id) = value
        queuedAjaxEvents push e
        ()
      }
      def doAjax(pageId: String) = {
        val q = queuedAjaxEvents
        queuedAjaxEvents = JArray()
//        val s = JSON.stringify
      }
    }
  }.asString)
}
