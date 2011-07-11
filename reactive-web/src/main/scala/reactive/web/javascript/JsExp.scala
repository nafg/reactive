package reactive
package web
package javascript

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
  implicit def any2JsExp[T, J <: JsAny, Exp[J <: JsAny] <: JsExp[J]](x: T)(implicit conv: ToJs[T, J, Exp]): Exp[J] = conv(x)

  implicit def canForward[T, J <: JsAny](implicit conv: ToJs.From[T]#To[J, JsExp]) = new CanForward[$[J =|> JsVoid], T] {
    def forward(source: Forwardable[T], target: => $[J =|> JsVoid])(implicit o: Observing) =
      source.foreach{ v => Reactions.queue(target apply conv(v)) }
  }
}

trait JsExp[+T <: JsAny] {
  def render: String

  def apply[P <: JsAny, R <: JsAny](p: JsExp[P])(implicit canApply: CanApply1[T, P, R]): JsExp[R] = canApply(this, p)

  def ->[T2 <: JsAny](exp: JsExp[T2])(implicit canSelect: CanSelect[T, T2]): JsExp[T2] = canSelect(this, exp)

  def +[T2 <: JsAny, R <: JsAny](that: JsExp[T2])(implicit canPlus: CanPlus[T, T2, R]): JsExp[R] = canPlus(this, that)
  def &[T2 <: JsAny, R <: JsAny](that: $[T2])(implicit can_& : Can_&[T, T2, R]): $[R] = can_&(this, that)
  def ||[T2 <: JsAny, R <: JsAny](that: $[T2]): $[R] = JsOp(this, that, "||")
  def !=[T2 <: JsAny](that: $[T2]): $[JsBoolean] = JsOp(this, that, "!=")
  def ==[T2 <: JsAny](that: $[T2]): $[JsBoolean] = JsOp(this, that, "==")
}

trait JsIdent[T <: JsAny] extends JsExp[T] {
  def ident: Symbol
  def render = ident.name
}
object JsIdent {
  object counter extends net.liftweb.http.RequestVar(0) {
    def next = { set(is + 1); is - 1 }
  }
  def fresh[T <: JsAny] = JsIdent[T](Symbol("x$"+counter.next))
  def apply[T <: JsAny](id: Symbol) = new JsIdent[T] { def ident = id }
}
case class JsIdentable(symbol: Symbol) {
  def $[J <: JsAny] = JsIdent[J](symbol)
}
trait JsLiteral[+T <: JsAny] extends JsExp[T]
object JsLiteral {
  def apply[T, J <: JsAny](x: T)(implicit conv: ToJsLit[T, J]): JsLiteral[J] = conv(x)
}
case class JsLiterable[T](x: T) {
  def $[J <: JsAny](implicit conv: ToJsLit[T, J]): JsLiteral[J] = conv(x)
}

class JsRaw[T <: JsAny](rendering: => String) extends JsExp[T] {
  def render = rendering
}
object JsRaw {
  def apply[T <: JsAny](rendering: => String) = new JsRaw[T](rendering)
}

trait ToJs[-S, J <: JsAny, +E[J <: JsAny] <: JsExp[J]] extends (S => E[J])
class ToJsExp[-S, J <: JsAny](renderer: S => String) extends ToJs[S, J, JsExp] {
  def apply(s: S) = JsRaw[J](renderer(s))
}
class ToJsLit[-S, J <: JsAny](renderer: S => String) extends ToJs[S, J, JsLiteral] {
  def apply(s: S) = new JsLiteral[J] { def render = renderer(s) }
}

trait ToJsLow { // make sure Map has a higher priority than a regular function
  implicit def func1[P <: JsAny, R <: JsAny]: ToJsLit[JsExp[P] => JsExp[R], JsFunction1[P, R]] =
    new ToJsLit[JsExp[P] => JsExp[R], JsFunction1[P, R]]("function(arg){return "+_($('arg)).render+"}")
}
object ToJs extends ToJsLow {
  class From[S] {
    type To[J <: JsAny, E[J <: JsAny] <: JsExp[J]] = ToJs[S, J, E]
  }
  class To[J <: JsAny, E[J <: JsAny] <: JsExp[J]] {
    type From[S] = ToJs[S, J, E]
  }

  implicit val double: From[Double]#To[JsNumber, JsLiteral] = new ToJsLit[Double, JsNumber](_.toString)
  implicit val int: ToJsLit[Int, JsNumber] = new ToJsLit[Int, JsNumber](_.toString)
  implicit val bool = new ToJsLit[Boolean, JsBoolean](_.toString)
  implicit val string: ToJsLit[String, JsString] = new ToJsLit[String, JsString]("\""+_+"\"")
  implicit val date = new ToJsLit[java.util.Date, JsDate]("new Date(\""+_.toString+"\")")
  implicit val regex = new ToJsLit[scala.util.matching.Regex, JsRegex]("/"+_.toString+"/")
  implicit val obj = new ToJsLit[Map[String, JsExp[_]], JsObj](_.map { case (k, v) => "\""+k+"\":"+v.render }.mkString("{", ",", "}"))
  implicit val array = new ToJsLit[List[JsExp[_]], JsArray](_.map(_.render).mkString("[", ",", "]"))
}

trait NamedIdent[T <: JsAny] extends JsIdent[T] {
  def ident = Symbol(classToIdent(getClass))
}
class JsVar[T <: JsAny, S: ToJs.To[T, JsExp]#From](init: S) extends NamedIdent[T]

object JsOp {
  def apply[L <: JsAny, R <: JsAny, T <: JsAny](l: $[L], r: $[R], op: String) = new JsOp[L, R, T](l, r, op)
}
class JsOp[-L <: JsAny, -R <: JsAny, +T <: JsAny](left: $[L], right: $[R], op: String) extends $[T] {
  def render = left.render + op + right.render
}

trait CanPlusLow {
  implicit def stringadd[L <: JsAny, R <: JsAny]: CanPlus[L, R, JsString] = new CanPlus((l: JsExp[L], r: JsExp[R]) => JsOp(l, r, "+"))
}
object CanPlus extends CanPlusLow {
  implicit val numNum: CanPlus[JsNumber, JsNumber, JsNumber] = new CanPlus((l: JsExp[JsNumber], r: JsExp[JsNumber]) => JsOp(l, r, "+"))
}
class CanPlus[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: (JsExp[L], JsExp[R]) => JsExp[T]) extends CanOp[L, R, T](f)

trait CanAmpLow {
  implicit def boolean[L <: JsAny, R <: JsAny] = new Can_&[L, R, JsBoolean](JsOp(_, _, "&"))
}
object Can_& extends CanAmpLow {
  implicit val numNum: Can_&[JsNumber, JsNumber, JsNumber] = new Can_&[JsNumber, JsNumber, JsNumber]((l, r) => JsOp(l, r, "&"))
}
class Can_&[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: ($[L], $[R]) => $[T]) extends CanOp[L, R, T](f)

class CanOp[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: ($[L], $[R]) => $[T]) extends (($[L], $[R]) => $[T]) {
  def apply(left: $[L], right: $[R]) = f(left, right)
}

object CanApply1 {
  implicit def canApply1[P <: JsAny, R <: JsAny]: CanApply1[P =|> R, P, R] = new CanApply1[P =|> R, P, R](
    f => p => {
      new JsRaw[R](f.render+"("+p.render+")") with JsStatement {
        def toReplace = List(p)
      }
    }
  )
}
class CanApply1[-T <: JsAny, -P <: JsAny, +R <: JsAny](r: JsExp[T] => JsExp[P] => JsExp[R]) {
  def apply(f: JsExp[T], p: JsExp[P]): JsExp[R] = r(f)(p)
}

object CanSelect {
  implicit def canSelect[T <: JsObj, T2 <: JsAny]: CanSelect[T, T2] = new CanSelect(
    o => m => JsRaw[T2](o.render+"."+m.render)
  )
}
class CanSelect[-T <: JsAny, T2 <: JsAny](f: JsExp[T] => JsExp[T2] => JsExp[T2]) {
  def apply(o: JsExp[T], m: JsExp[T2]): JsExp[T2] = f(o)(m)
}

trait JsStub extends NamedIdent[JsObj]

trait JsStatement {
  def render: String
  def toReplace: List[$[_]]

  for (e <- toReplace if e.isInstanceOf[JsStatement] && (e eq JsStatement.peek)) JsStatement.pop
  JsStatement.push(this)
}
object JsStatement {
  val stack = new scala.util.DynamicVariable[List[List[JsStatement]]](List(Nil))
  def currentScope: List[JsStatement] = stack.value.head
  def currentScope_=(ss: List[JsStatement]) = stack.value = ss :: stack.value.tail
  def topScope = stack.value.tail.isEmpty
  def inScope(p: => Unit): List[JsStatement] = stack.withValue(Nil :: stack.value){
    p
    currentScope
  }

  def push(s: JsStatement) = {
    currentScope ::= s
  }
  def pop: JsStatement = {
    val ret = currentScope.head
    currentScope = currentScope.tail
    ret
  }
  def peek = currentScope.head
  def replace(pf: PartialFunction[JsStatement, JsStatement]) =
    if (pf.isDefinedAt(peek)) push(pf(pop))
}
