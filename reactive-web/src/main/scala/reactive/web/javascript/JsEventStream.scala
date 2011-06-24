package reactive
package web
package javascript

import net.liftweb.http.js.JsCmds.Run

import JsTypes._


class JsEventStream[T<:JsAny]()(implicit page: Page) {parent =>
  lazy val id = page.nextId
  var rendered = false
  def renderExp = "new EventStream()"
  def render = if(!rendered){
    rendered = true
    JsEventStream.render // define the constructor
    Reactions.queue(Run("window."+id+"=" + renderExp))
  }
  protected def child[U <: JsAny](renderer: =>String) = {
    render
    new JsEventStream[U]()(page) {
      override def renderExp: String = renderer
    }
  }
  def fire(v: JsExp[_]) {
    Reactions.queue(Run(id+".fire("+v.render+")"))
  }

  def foreach[F : ToJs.To[JsFunction1[T,JsVoid]]#From](f: F) {
    render
    Reactions.queue(Run(id+".foreach("+f.render+")"))
  }
  //TODO combine queued ajax calls
  //TODO type safety
  def foreach(f: String=>Unit) {
    import net.liftweb.http.{S,SHtml}
    val ajaxCall: JsExp[T]=>JsExp[JsVoid] = value => JsRaw[JsVoid](S.fmapFunc(S.contextFuncBuilder(RElem.ajaxFunc(f))) {funcId =>
      SHtml.makeAjaxCall(
        net.liftweb.http.js.JE.JsRaw("'"+funcId+"=' + encodeURIComponent("+value+")")
      ).toJsCmd
    })
    foreach(ajaxCall)(ToJs.func1[T,JsVoid])
  }
  def map[U<:JsAny, F <% JsExp[JsFunction1[T,U]]](f: F): JsEventStream[U] = child("parent.map("+f.render+")")
  def flatMap[U<:JsAny, F <% JsExp[JsFunction1[T,U]]](f: F): JsEventStream[U] = child("parent.flatMap("+f.render+")")
  def filter[F <% JsExp[JsFunction1[T,JsBoolean]]](f: F): JsEventStream[T] = child("parent.filter("+f.render+")")
//  def takeWhile(p: T=>Boolean): EventStream[T]
//  def foldLeft[U](initial: U)(f: (U,T)=>U): EventStream[U]
//  def |[U>:T](that: EventStream[U]): EventStream[U]
//  def hold[U>:T](init: U): Signal[U]
//  
//  def nonrecursive: EventStream[T]
  
}


object JsEventStream extends net.liftweb.http.RequestVar(false) {
  def render = if(!is) {
    set(true)
    Reactions.queue(Run(
"""window.EventStream = function() {
  this.listeners = []
  this.foreach = function(f){this.listeners.push(f)}
  this.addListener = this.foreach
  this.removeListener = function(f){
    for(l in this.listeners) {
      if(this.listeners[l] === f) {
        delete this.listeners[l]
        break
      }
    }
  }
  this.fire = function(v) {
    for(l in this.listeners) {
      this.listeners[l](v)
    }
  }
  this.map = function(f) {
    var mapped = new EventStream()
    this.addListener(function(v){mapped.fire(f(v))})
    return mapped
  }
  this.flatMap = function(f) {
    var flatMapped = new EventStream()
    var lastES = null
    this.addListener(function(v){
      if(lastES) lastES.removeListener(flatMapped.fire)
      lastES = f(v)
      lastES.addListener(flatMapped.fire)
    })
    return flatMapped
  }
  this.filter = function(f) {
    var filtered = new EventStream()
    this.addListener(function(v){
      if(f(v)) filtered.fire(v)
    })
    return filtered
  }
  return this
}
"""
    ))
  }
}
