package reactive
package web
package javascript

import net.liftweb.json.{ Formats, DefaultFormats }

import JsTypes._

object JsEventStream {
  implicit def canForward[T, J <: JsAny](implicit conv: ToJs.From[T]#To[J, JsExp]): CanForward[JsEventStream[J], T] = new CanForward[JsEventStream[J], T] {
    def forward(source: Forwardable[T, _], target: => JsEventStream[J])(implicit o: Observing) =
      source foreach { v => target.fire(conv(v)) }
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
      Reactions.inAnyScope(page) {
        Reactions queue render+"="+initExp
      }
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
    Reactions.inAnyScope(page) {
      Reactions queue JsExp.render(fireExp(v))
      Reactions queue "window.setTimeout('reactive.doAjax()',500)"
    }
  }

  protected[reactive] def foreachImpl(f: $[T =|> JsVoid]) {
    Reactions.inAnyScope(page) {
      init
      Reactions queue render+".foreach("+JsExp.render(f)+")"
    }
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
