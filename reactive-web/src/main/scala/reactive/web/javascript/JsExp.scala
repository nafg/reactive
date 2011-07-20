package reactive
package web
package javascript

/**
 * Contains types that model javascript types.
 * These classes cannot be instantiated.
 * They are used as type parameters for JsExp etc.
 */
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

/**
 * A Scala representation of a javscript expression.
 * @tparam T the javascript type of the expression.
 */
trait JsExp[+T <: JsAny] {
  /**
   * Returns the javascript representation of the expression in a String
   */
  def render: String

  /**
   * Returns a JsExp that represents function application of this JsExp
   */
  def apply[P <: JsAny, R <: JsAny](p: JsExp[P])(implicit canApply: CanApply1[T, P, R]): JsExp[R] with JsStatement = canApply(this, p)

  /**
   * Returns a JsExp that represents member selection (the period) of this JsExp.
   * A better solution is to use JsStub
   */
  def ->[T2 <: JsAny](exp: JsExp[T2])(implicit canSelect: CanSelect[T, T2]): JsExp[T2] = canSelect(this, exp)

  /**
   * Returns a JsExp that represents this + that
   */
  def +[T2 <: JsAny, R <: JsAny](that: JsExp[T2])(implicit canPlus: CanPlus[T, T2, R]): JsExp[R] = canPlus(this, that)
  /**
   * Returns a JsExp that represents this & that
   */
  def &[T2 <: JsAny, R <: JsAny](that: $[T2])(implicit can_& : Can_&[T, T2, R]): $[R] = can_&(this, that)
  /**
   * Returns a JsExp that represents this || that
   */
  def ||[T2 <: JsAny, R <: JsAny](that: $[T2]): $[R] = JsOp(this, that, "||")
  /**
   * Returns a JsExp that represents this != that
   */
  def !=[T2 <: JsAny](that: $[T2]): $[JsBoolean] = JsOp(this, that, "!=")
  /**
   * Returns a JsExp that represents this == that
   */
  def ==[T2 <: JsAny](that: $[T2]): $[JsBoolean] = JsOp(this, that, "==")
}

/**
 * A JsExp that represents a reference to an existing, named identifier
 */
trait JsIdent[T <: JsAny] extends JsExp[T] {
  def ident: Symbol
  def render = ident.name
}
object JsIdent {
  object counter extends net.liftweb.http.RequestVar(0) {
    def next = { set(is + 1); is - 1 }
  }
  /**
   * Returns a JsIdent with a fresh name
   */
  def fresh[T <: JsAny] = JsIdent[T](Symbol("x$"+counter.next))
  /**
   * Returns a JsIdent with the specified name
   */
  def apply[T <: JsAny](id: Symbol) = new JsIdent[T] { def ident = id }
}
case class JsIdentable(symbol: Symbol) {
  def $[J <: JsAny] = JsIdent[J](symbol)
}
/**
 * A JsExp that represents a literal value
 */
trait JsLiteral[+T <: JsAny] extends JsExp[T]
object JsLiteral {
  def apply[T, J <: JsAny](x: T)(implicit conv: ToJsLit[T, J]): JsLiteral[J] = conv(x)
}
case class JsLiterable[T](x: T) {
  def $[J <: JsAny](implicit conv: ToJsLit[T, J]): JsLiteral[J] = conv(x)
}

/**
 * A JsExp whose javascript representation is specified directly
 */
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
/**
 * Contains implicit conversions from scala values to javascript literals
 */
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

/**
 * A JsIdent whose javascript name is the scala type
 */
trait NamedIdent[T <: JsAny] extends JsIdent[T] {
  def ident = Symbol(scalaClassName(getClass))
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
class CanApply1[-T <: JsAny, -P <: JsAny, +R <: JsAny](r: JsExp[T] => JsExp[P] => (JsExp[R] with JsStatement)) {
  def apply(f: JsExp[T], p: JsExp[P]): JsExp[R] with JsStatement = r(f)(p)
}

object CanSelect {
  implicit def canSelect[T <: JsObj, T2 <: JsAny]: CanSelect[T, T2] = new CanSelect(
    o => m => JsRaw[T2](o.render+"."+m.render)
  )
}
class CanSelect[-T <: JsAny, T2 <: JsAny](f: JsExp[T] => JsExp[T2] => JsExp[T2]) {
  def apply(o: JsExp[T], m: JsExp[T2]): JsExp[T2] = f(o)(m)
}

/**
 * Traits that extends JsStub can have proxy instances vended
 * whose methods result in calls to javascript methods
 */
trait JsStub extends NamedIdent[JsObj]

/**
 * A scala representation of a javascript statement.
 * On instantiation, puts itself on the JsStatement stack.
 */
trait JsStatement {
  /**
   * Returns the javascript representation in a String
   */
  def render: String
  /**
   * A list of javascript expressions, that if they are a JsStatement
   * and on the top of the JsStatement stack, should be replaced.
   */
  def toReplace: List[$[_]]

  for (e <- toReplace if e.isInstanceOf[JsStatement] && (e eq JsStatement.peek)) JsStatement.pop

  JsStatement.push(this)
}
/**
 * Maintains a thread-local stack of statement blocks
 */
object JsStatement {
  /**
   * A dynamically-scoped stack of blocks of JsStatements
   */
  val stack = new scala.util.DynamicVariable[List[List[JsStatement]]](List(Nil))
  /**
   * The top JsStatement block
   */
  def currentScope: List[JsStatement] = stack.value.head
  /**
   * Sets the top JsStatement block
   */
  def currentScope_=(ss: List[JsStatement]) = stack.value = ss :: stack.value.tail
  /**
   * Returns true if there is no other statement block on the stack
   */
  def bottomScope = stack.value.tail.isEmpty
  /**
   * Evaluates p in a new scope, and returns the top of the stack
   * (JsStatements pushed during evaluation of p)
   */
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
