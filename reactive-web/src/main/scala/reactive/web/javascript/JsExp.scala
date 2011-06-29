package reactive
package web
package javascript

import net.liftweb.common.HLists._

object JsTypes {
  class JsAny protected ;
  final class JsBoolean private extends JsAny
  final class JsNumber private extends JsAny
  final class JsString private extends JsAny
  final class JsDate private extends JsAny
  final class JsRegex private extends JsAny
  final class JsObj private extends JsAny
  final class JsArray private extends JsAny
  final class JsFunction1[-P <: JsAny, +R <: JsAny] private extends JsAny
  final class JsVoid private extends JsAny

}
import JsTypes._

object JsExp {
  implicit def any2JsExp[T, J <: JsAny,Exp[J<:JsAny]<:JsExp[J]](x: T)(implicit conv: ToJs[T, J, Exp]): Exp[J] = conv(x)
}

trait JsExp[+T <: JsAny] {
  def render: String

  def +[T2 <: JsAny, R <: JsAny](that: JsExp[T2])(implicit canPlus: CanPlus[T, T2, R]): JsExp[R] = canPlus(this, that)

  def apply[P <: JsAny, R <: JsAny](p: JsExp[P])(implicit canApply: CanApply1[T, P, R]): JsExp[R] = canApply(this, p)
  
  def ->[T2 <: JsAny](exp: JsExp[T2])(implicit canSelect: CanSelect[T,T2]): JsExp[T2] = canSelect(this, exp)
}

case class JsIdent[T <: JsAny](ident: Symbol) extends JsExp[T] {
  def render = ident.name
}
object JsIdent {
  object counter extends net.liftweb.http.RequestVar(0) {
    def next = {set(is+1); is-1}
  }
  def fresh[T<:JsAny] = JsIdent[T](Symbol("x$"+counter.next))
}
trait JsLiteral[+T <: JsAny] extends JsExp[T]
object JsLiteral {
  def apply[T, J <: JsAny](x: T)(implicit conv: ToJsLit[T, J]): JsLiteral[J] = conv(x)
}
case class JsLiterable[T](x: T) {
  def $[J <: JsAny](implicit conv: ToJsLit[T, J]): JsLiteral[J] = conv(x)
}

trait JsRaw[T <: JsAny] extends JsExp[T]
object JsRaw {
  def apply[T <: JsAny](rendering: => String) = new JsRaw[T] {
    def render = rendering
  }
}

trait ToJs[-S, J <: JsAny, +E[J<:JsAny]<:JsExp[J]] extends (S => E[J])
class ToJsExp[-S, J<:JsAny](renderer: S=>String) extends ToJs[S,J,JsExp] {
  def apply(s: S) = new JsRaw[J] {
    def render = renderer(s)
  }
}
class ToJsLit[-S,J<:JsAny](renderer: S=>String) extends ToJs[S,J,JsLiteral] {
  def apply(s: S) = new JsLiteral[J] {def render = renderer(s)}
}

trait ToJsLow { // make sure Map has a higher priority than a regular function
  implicit def func1[P <: JsAny, R <: JsAny]: ToJsLit[JsExp[P] => JsExp[R], JsFunction1[P, R]] =
    new ToJsLit[JsExp[P] => JsExp[R], JsFunction1[P, R]]("function(arg){"+_(JsIdent('arg)).render+"}")
}
object ToJs extends ToJsLow{
  class From[S] {
    type To[J <: JsAny, E[J<:JsAny]<:JsExp[J]] = ToJs[S, J, E]
  }
  class To[J <: JsAny, E[J<:JsAny]<:JsExp[J]] {
    type From[S] = ToJs[S, J, E]
  }

  implicit val number: From[Double]#To[JsNumber,JsLiteral] = new ToJsLit[Double, JsNumber](_.toString)
  implicit val bool = new ToJsLit[Boolean, JsBoolean](_.toString)
  implicit val string: ToJsLit[String, JsString] = new ToJsLit[String, JsString]("\""+_+"\"")
  implicit val date = new ToJsLit[java.util.Date, JsDate]("new Date(\""+_.toString+"\")")
  implicit val regex = new ToJsLit[scala.util.matching.Regex, JsRegex]("/"+_.toString+"/")
  implicit val obj = new ToJsLit[Map[String, JsExp[_]], JsObj](_.map { case (k, v) => "\""+k+"\":"+v.render }.mkString("{", ",", "}"))
  implicit val array = new ToJsLit[List[JsExp[_]], JsArray](_.map(_.render).mkString("[", ",", "]"))
}

class JsDef[T <: JsAny, S: ToJs.To[T,JsExp]#From](init: S) extends JsExp[T] {
  def name: Symbol = Symbol(getClass.getName.toList.reverse.dropWhile('$'==).takeWhile('$'!=).reverse.mkString)
  def render = "var "+name.name+"="+init.render+";"
}

trait CanPlusLow {
  implicit def stringadd[L <: JsAny, R <: JsAny]: CanPlus[L, R, JsString] = new CanPlus((l: JsExp[L], r: JsExp[R]) => JsRaw(l.render+"+"+r.render))
}
object CanPlus extends CanPlusLow {
  implicit val numNum: CanPlus[JsNumber, JsNumber, JsNumber] = new CanPlus((l: JsExp[JsNumber], r: JsExp[JsNumber]) => JsRaw(l.render+"+"+r.render))
}
class CanPlus[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: (JsExp[L], JsExp[R]) => JsExp[T]) {
  def apply(left: JsExp[L], right: JsExp[R]): JsExp[T] = f(left, right)
}

object CanApply1 {
  implicit def canApply1[P <: JsAny, R <: JsAny]: CanApply1[P =|> R, P, R] = new CanApply1[P =|> R, P, R](
    f => p => JsRaw[R](f.render+"("+p.render+")")
  )
}
class CanApply1[-T <: JsAny, -P <: JsAny, +R <: JsAny](r: JsExp[T] => JsExp[P] => JsExp[R]) {
  def apply(f: JsExp[T], p: JsExp[P]): JsExp[R] = r(f)(p)
}

object CanSelect {
  implicit def canSelect[T<:JsObj, T2<:JsAny]: CanSelect[T,T2] = new CanSelect(
    o => m => JsRaw[T2](o.render+"."+m.render)
  )
}
class CanSelect[-T <: JsAny, T2 <: JsAny](f: JsExp[T]=>JsExp[T2]=>JsExp[T2]) {
  def apply(o: JsExp[T], m: JsExp[T2]): JsExp[T2] = f(o)(m)
}
