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
  case class NumericOps(left: JsExp[JsNumber]) {
    def +(right: JsExp[JsNumber]): JsExp[JsNumber] = JsRaw[JsNumber](left.render+"+"+right.render)
  }
  case class StringOps(left: JsExp[JsString]) {
    def &(right: JsExp[JsString]): JsExp[JsString] = JsRaw[JsString](left.render+"+"+right.render)
  }
  case class Applicable[-P<:JsAny,+R<:JsAny](f: JsExp[JsFunction1[P,R]]) extends (JsExp[P]=>JsExp[R]) {
    def apply(p: JsExp[P]): JsExp[R] = JsRaw[R](f.render+"("+p.render+")")
  }
  
  implicit def num2ops[T <% JsExp[JsNumber]](left: T): NumericOps = NumericOps(left)
  implicit def string2ops[T <% JsExp[JsString]](left: T): StringOps = StringOps(left)
  
  implicit def func1toApplicable[P<:JsAny,R<:JsAny](f: JsExp[JsFunction1[P,R]]): Applicable[P,R] = Applicable[P,R](f)

  implicit def any2JsExp[T, J <: JsAny](x: T)(implicit conv: ToJs[T, J]): JsExp[J] = conv(x)
}

trait JsExp[+T <: JsAny] {
  def render: String
}
case class JsIdent[T <: JsAny](ident: Symbol) extends JsExp[T] {
  def render = ident.name
}
trait JsLiteral[+T <: JsAny] extends JsExp[T]
object JsLiteral {
  def apply[T, J <: JsAny](x: T)(implicit conv: ToJs[T, J]): JsLiteral[J] = conv(x)
}
case class JsLiterable[T](x: T) {
  def j[J <: JsAny](implicit conv: ToJs[T, J]): JsLiteral[J] = conv(x)
}

trait JsRaw[T <: JsAny] extends JsExp[T]
object JsRaw {
  def apply[T <: JsAny](rendering: => String) = new JsRaw[T] {
    def render = rendering
  }
}

class ToJs[-S, J <: JsAny](renderer: S => String) extends (S => JsLiteral[J]) {
  def apply(s: S): JsLiteral[J] = new JsLiteral[J] {
    def render = renderer(s)
  }
}
object ToJs {
  class From[S] {
    type To[J <: JsAny] = ToJs[S, J]
  }
  class To[J <: JsAny] {
    type From[S] = ToJs[S, J]
  }

  implicit val number: From[Double]#To[JsNumber] = new ToJs[Double, JsNumber](_.toString)
  implicit val bool = new ToJs[Boolean, JsBoolean](_.toString)
    implicit val string: ToJs[String,JsString] = new ToJs[String, JsString]("\"" + _ + "\"")
//  implicit val jlString: ToJs[java.lang.String, JsString] = new ToJs[java.lang.String, JsString]("\""+_+"\"")
  implicit val date = new ToJs[java.util.Date, JsDate]("new Date(\""+_.toString+"\")")
  implicit val regex = new ToJs[scala.util.matching.Regex, JsRegex]("/"+_.toString+"/")
  implicit val obj = new ToJs[Map[String, JsExp[_]], JsObj](_.map { case (k, v) => "\""+k+"\":"+v.render }.mkString("{", ",", "}"))
  implicit val array = new ToJs[List[JsExp[_]], JsArray](_.map(_.render).mkString("[", ",", "]"))
  implicit def func1[P <: JsAny, R <: JsAny]: ToJs[JsExp[P] => JsExp[R], JsFunction1[P, R]] =
    new ToJs[JsExp[P] => JsExp[R], JsFunction1[P, R]]("function(arg){"+_(JsIdent('arg)).render+"}")
}

class JsDef[T <: JsAny, S : ToJs.To[T]#From](init: S) extends JsExp[T] {
  def name: Symbol = Symbol(getClass.getName.toList.reverse.dropWhile('$'==).takeWhile('$'!=).reverse.mkString)
  def render = "var "+name.name+"="+init.render+";"
}

