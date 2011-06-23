package reactive
package web
package javascript

import net.liftweb.http.js.JsCmds.Run



trait JsExp[T <: JsAny] {
  def render: String
}
case class JsVar[T <: JsAny](ident: Symbol) extends JsExp[T] {
  def render = ident.toString
}
trait JsLiteral[T <: JsAny] extends JsExp[T]
object JsLiteral {
  def apply[T, J <: JsAny](x: T)(implicit conv: JsConversion[J, T]): JsLiteral[J] = conv(x)
}
trait JsRaw[T <: JsAny] extends JsExp[T]
object JsRaw {
  def apply[T <: JsAny](rendering: =>String) = new JsRaw[T] {
    def render = rendering
  }
}
trait JsAny
trait JsBoolean extends JsAny
trait JsNumber extends JsAny
trait JsString extends JsAny
trait JsDate extends JsAny
trait JsRegex extends JsAny
trait JsObj extends JsAny
trait JsArray extends JsAny
trait JsFunction[P<:JsAny,R<:JsAny] extends JsAny

class JsConversion[J <: JsAny, -S](renderer: S=>String) extends (S=>JsLiteral[J]) {
  def apply(s: S): JsLiteral[J] = new JsLiteral[J] {
    def render = renderer(s)
  }
}
object JsConversion {
  implicit val number = new JsConversion[JsNumber, Double](_.toString)
  implicit val bool = new JsConversion[JsBoolean, Boolean](_.toString)
  implicit val string = new JsConversion[JsString, String]("\"" + _ + "\"")
  implicit val date = new JsConversion[JsDate, java.util.Date]("new Date(\"" + _.toString + "\")")
  implicit val regex = new JsConversion[JsRegex, scala.util.matching.Regex]("/" + _.toString + "/")
  implicit val obj = new JsConversion[JsObj, Map[String,JsExp[_]]](_.map{case (k,v)=>"\""+k+"\":"+v.render}.mkString("{",",","}"))
  implicit val array = new JsConversion[JsArray, List[JsExp[_]]](_.map(_.render).mkString("[",",","]"))
  implicit def func[P<:JsAny,R<:JsAny] = new JsConversion[JsFunction[P,R], JsExp[P]=>JsExp[R]]("function(arg){" + _(JsVar('arg)).render + "}")
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
  def foreach[F <% JsExp[JsFunction[T,JsAny]]](f: F) {
    render
    Reactions.queue(Run(id+".foreach("+f.render+")"))
  }
  //TODO combine queued ajax calls
  //TODO type safety
  def foreach(f: String=>Unit) {
    import net.liftweb.http.{S,SHtml}
    val ajaxCall: JsExp[T]=>JsExp[JsAny] = value => JsRaw[JsAny](S.fmapFunc(S.contextFuncBuilder(RElem.ajaxFunc(f))) {funcId =>
      SHtml.makeAjaxCall(
        net.liftweb.http.js.JE.JsRaw("'"+funcId+"=' + encodeURIComponent("+value+")")
      ).toJsCmd
    })
    foreach(ajaxCall)(JsConversion.func[T,JsAny])
  }
  def map[U<:JsAny, F <% JsExp[JsFunction[T,U]]](f: F): JsEventStream[U] = child("parent.map("+f.render+")")
  def flatMap[U<:JsAny, F <% JsExp[JsFunction[T,U]]](f: F): JsEventStream[U] = child("parent.flatMap("+f.render+")")
  def filter[F <% JsExp[JsFunction[T,JsBoolean]]](f: F): JsEventStream[T] = child("parent.filter("+f.render+")")
//  def takeWhile(p: T=>Boolean): EventStream[T]
//  def foldLeft[U](initial: U)(f: (U,T)=>U): EventStream[U]
//  def |[U>:T](that: EventStream[U]): EventStream[U]
//  def hold[U>:T](init: U): Signal[U]
//  
//  def nonrecursive: EventStream[T]
  
}
