package reactive
package web
package javascript

import net.liftweb.http.js.JsCmds.Run

import JsTypes._

//TODO use PageIds
class JsEventStream[T <: JsAny]()(implicit page: Page) { parent =>
  lazy val id = page.nextId
  var rendered = false
  def renderExp = "new EventStream()"
  def accessExp = "reactive.eventStreams['"+id+"']"
  def render = synchronized {
    if (!rendered) {
      rendered = true
      Reactions.queue(Run(accessExp+"="+renderExp))
    }
  }

  protected def child[U <: JsAny](renderer: => String) = {
    render
    new JsEventStream[U]()(page) {
      override def renderExp: String = renderer
    }
  }
  def fireExp(v: $[_]): $[JsVoid] = JsRaw{
    render
    accessExp+".fire("+v.render+");"
  }
  def fire(v: JsExp[_]) {
    Reactions queue Run(fireExp(v).render)
    Reactions queue Run("window.setTimeout('reactive.doAjax()',500)")
  }

  private def foreachImpl(f: $[T =|> JsVoid]) {
    render
    Reactions queue Run(accessExp+".foreach("+f.render+")")
  }
  def foreach[E[J <: JsAny] <: JsExp[J], F: ToJs.To[JsFunction1[T, JsVoid], E]#From](f: F) {
    foreachImpl(f)
  }
  def foreach(f: $[T =|> JsVoid]) {
    foreachImpl(f)
  }
  //TODO encoder
  def toServer[U](extract: net.liftweb.json.JValue => U): EventStream[U] = {
    foreach(JsRaw[T =|> JsVoid]("reactive.queueAjax('"+id+"')"))
    page.ajaxEvents.collect { case (`id`, json) => extract(json) }
  }
  def map[U <: JsAny, F <% JsExp[JsFunction1[T, U]]](f: F): JsEventStream[U] = child("parent.map("+f.render+")")
  def flatMap[U <: JsAny, F <% JsExp[JsFunction1[T, U]]](f: F): JsEventStream[U] = child("parent.flatMap("+f.render+")")
  def filter[F <% JsExp[JsFunction1[T, JsBoolean]]](f: F): JsEventStream[T] = child("parent.filter("+f.render+")")
  //  def takeWhile(p: T=>Boolean): EventStream[T]
  //  def foldLeft[U](initial: U)(f: (U,T)=>U): EventStream[U]
  //  def |[U>:T](that: EventStream[U]): EventStream[U]
  //  def hold[U>:T](init: U): Signal[U]
  //  
  //  def nonrecursive: EventStream[T]

}

