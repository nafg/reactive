package reactive
package web
package javascript

import net.liftweb.json._

import java.lang.reflect.{ InvocationHandler, Proxy, Method }

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
  final class JsArray[+T <: JsAny] private extends JsAny
  final class JsFunction0[+R <: JsAny] private extends JsAny
  final class JsFunction1[-P <: JsAny, +R <: JsAny] private extends JsAny
  final class JsFunction2[-P1 <: JsAny, -P2 <: JsAny, +R <: JsAny] private extends JsAny
  final class JsVoid private extends JsAny
}

import JsTypes._

object JsExp extends ToJsHigh {
  implicit def canForward[T, J <: JsAny](implicit conv: ToJs.From[T]#To[J, JsExp]) = new CanForward[$[J =|> JsVoid], T] {
    def forward(source: Forwardable[T], target: => $[J =|> JsVoid])(implicit o: Observing) =
      source.foreach{ v => Reactions.queue(target apply conv(v)) }
  }

  /**
   * Returns the javascript representation of the expression in a String
   */
  def render(e: JsExp[_ <: JsAny]): String = e match {
    case null => "null"
    case e    => e.render
  }
}

/**
 * A Scala representation of a javscript expression.
 * @tparam T the javascript type of the expression.
 */
trait JsExp[+T <: JsAny] {
  def render: String

  /**
   * Returns a JsExp that represents function application of this JsExp
   */
  def apply[R <: JsAny]()(implicit canApply: CanApply[T, Unit, R]): JsExp[R] with JsStatement = canApply(this, ())
  /**
   * Returns a JsExp that represents function application of this JsExp
   */
  def apply[P <: JsAny, R <: JsAny](p: JsExp[P])(implicit canApply: CanApply[T, JsExp[P], R]): JsExp[R] with JsStatement = canApply(this, p)
  /**
   * Returns a JsExp that represents function application of this JsExp
   */
  def apply[P1 <: JsAny, P2 <: JsAny, R <: JsAny](p1: JsExp[P1], p2: JsExp[P2])(implicit canApply: CanApply[T, (JsExp[P1], JsExp[P2]), R]): JsExp[R] with JsStatement = canApply(this, (p1, p2))

  /**
   * Array access
   */
  def get[I <: JsAny, R <: JsAny](i: JsExp[I])(implicit canGet: CanGet[T, I, R]): Assignable[R] = canGet(this, i)

  /**
   * Returns a JsExp that represents member selection (the period) of this JsExp.
   * A better solution is to use JsStub
   */
  def ->[T2 <: JsAny](exp: JsExp[T2])(implicit canSelect: CanSelect[T, T2]): JsExp[T2] = canSelect(this, exp)

  /**
   * Returns a JsExp that represents this + that
   */
  def +[T2 <: JsAny, R <: JsAny](that: JsExp[T2])(implicit canPlus: CanPlus[T, T2, R]): JsExp[R] = canPlus(this, that)
  def -[T2 <: JsAny, R <: JsAny](that: JsExp[T2])(implicit canMinus: CanMinus[T, T2, R]): JsExp[R] = canMinus(this, that)
  def *[T2 <: JsAny, R <: JsAny](that: JsExp[T2])(implicit canTimes: CanTimes[T, T2, R]): JsExp[R] = canTimes(this, that)
  def /[T2 <: JsAny, R <: JsAny](that: JsExp[T2])(implicit canDivide: CanDivide[T, T2, R]): JsExp[R] = canDivide(this, that)
  def %[T2 <: JsAny, R <: JsAny](that: JsExp[T2])(implicit canMod: CanMod[T, T2, R]): JsExp[R] = canMod(this, that)
  /**
   * Returns a JsExp that represents this & that
   */
  def &[T2 <: JsAny, R <: JsAny](that: $[T2])(implicit can_& : Can_&[T, T2, R]): $[R] = can_&(this, that)
  def |[T2 <: JsAny, R <: JsAny](that: $[T2])(implicit can_| : Can_|[T, T2, R]): $[R] = can_|(this, that)

  /**
   * Returns a JsExp that represents this || that
   */
  def ||[T2 <: JsBoolean, R <: JsAny](that: $[T2])(implicit ev: T <:< JsBoolean): $[R] = JsOp(this, that, "||")
  def &&[T2 <: JsBoolean, R <: JsAny](that: $[T2])(implicit ev: T <:< JsBoolean): $[R] = JsOp(this, that, "&&")

  /**
   * Returns a JsExp that represents this != that
   */
  def !==[T2 <: JsAny](that: $[T2]): $[JsBoolean] = JsOp(this, that, "!=")
  /**
   * Returns a JsExp that represents this == that
   */
  def ===[T2 <: JsAny](that: $[T2]): $[JsBoolean] = JsOp(this, that, "==")
  def ====[T2 <: JsAny](that: $[T2]): $[JsBoolean] = JsOp(this, that, "===")
  def !===[T2 <: JsAny](that: $[T2]): $[JsBoolean] = JsOp(this, that, "!==")

  def >[T2 <: JsAny](that: $[T2])(implicit canOrder: CanOrder[T, T2, JsBoolean]): $[JsBoolean] = canOrder(">")(this, that)
  def >=[T2 <: JsAny](that: $[T2])(implicit canOrder: CanOrder[T, T2, JsBoolean]): $[JsBoolean] = canOrder(">=")(this, that)
  def <[T2 <: JsAny](that: $[T2])(implicit canOrder: CanOrder[T, T2, JsBoolean]): $[JsBoolean] = canOrder("<")(this, that)
  def <=[T2 <: JsAny](that: $[T2])(implicit canOrder: CanOrder[T, T2, JsBoolean]): $[JsBoolean] = canOrder("<=")(this, that)

  def unary_! : JsExp[JsBoolean] = new JsRaw[JsBoolean]("(!"+JsExp.render(this)+")")
  def unary_-(implicit ev: T <:< JsNumber): JsExp[JsNumber] = new JsRaw[JsNumber]("(-"+JsExp.render(this)+")")
  def unary_~(implicit ev: T <:< JsNumber): JsExp[JsNumber] = new JsRaw[JsNumber]("(~"+JsExp.render(this)+")")
  def unary_+ : JsExp[JsNumber] = new JsRaw[JsNumber]("(+"+JsExp.render(this)+")")
}

/**
 * Adds javascript Array API to JsExp[JsArray[J]]
 * @tparam J the member type
 */
trait Array[J <: JsAny] extends JsStub {
  def push(x: JsExp[J]): JsExp[JsNumber]
  var length: JsExp[JsNumber]
}

object Array {
  implicit def fromJsType[J <: JsAny] = new Extend[JsExp[JsTypes.JsArray[J]], Array[J]]

  /**
   * DSL to create a JsExp[JsArray[_]]
   */
  def apply[A, J <: JsAny](elems: A*)(implicit conv: ToJsLit[A, J]): JsExp[JsArray[J]] = JsRaw(
    elems.map(conv(_).render).mkString("[", ",", "]")
  )
}
/**
 * DSL to create a JsExp[JsObj]
 */
object Object {
  def apply(props: JsProp*): JsExp[JsObj] = JsRaw[JsObj](
    props.map{ p =>
      "\""+p.key+"\":"+p.renderValue
    }.mkString("{", ",", "}")
  )
}
class JsProp(val key: String, v: => JsExp[JsAny]) {
  lazy val (value, statements) = JsStatement.inScope(v)
  def renderValue = {
    val rendered = statements.map(JsStatement.render) :+ JsExp.render(value)
    val noDup = rendered.splitAt(rendered.length - 2) match {
      case (pre, a :: b :: Nil) if a == b => pre :+ a
      case _                              => rendered
    }
    if (noDup.length == 1) noDup.head
    else noDup.mkString("{", ";", "}")
  }
}
object JsProp {
  implicit def pair2prop[A](pair: => (String, A))(implicit conv: ToJsLit[A, _ <: JsAny]): JsProp = new JsProp(pair._1, conv(pair._2))
  implicit def jspair2prop(pair: (String, JsExp[JsAny])): JsProp = new JsProp(pair._1, pair._2)
}

/**
 * A JsExp that represents a reference to an existing, named identifier
 */
trait JsIdent[T <: JsAny] extends JsExp[T] {
  def ident: Symbol
  def render = ident.name
}
object JsIdent {
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

/**
 * A typeclass to convert a scala value to a JsExp
 */
trait ToJs[-S, J <: JsAny, +E[K <: JsAny] <: JsExp[K]] extends (S => E[J])
class ToJsExp[-S, J <: JsAny](renderer: S => String) extends ToJs[S, J, JsExp] {
  def apply(s: S) = JsRaw[J](renderer(s))
}
class ToJsLit[-S, J <: JsAny](f: S => JsLiteral[J]) extends ToJs[S, J, JsLiteral] {
  def this(renderer: S => String, dummy: Unit*) = this({ s: S =>
    val r = renderer(s)
    new JsRaw[J](r) with JsLiteral[J]
  })
  def apply(s: S) = f(s)
}

abstract class FuncXLit[+R <: JsAny, +J <: JsAny](num: Int) extends JsLiteral[J] {
  def exp: JsExp[R]
  def statements: List[JsStatement]
  def render = {
    // if the last statement is a return statement, disregard the expression value
    object MIH {
      def unapply(x: JsExp[_]) =
        if (!Proxy.isProxyClass(x.getClass)) None
        else Some(Proxy.getInvocationHandler(x)).collect{ case mih: MethodInvocationHandler[_] => mih }
    }
    val args = (0 until num).map("arg"+).mkString("(", ",", ")")
    "(function"+args + JsStatement.renderBlock(
      (statements.lastOption, exp) match {
        case (Some(_: Return[_]), _)             => statements
        case (Some(s), e) if s eq e              => statements.dropRight(1) ++ JsStatement.inScope(Return(exp))._2
        case (Some(s), MIH(mih)) if mih.apm eq s => statements.dropRight(1) ++ JsStatement.inScope(Return(exp))._2
        case _                                   => statements ++ JsStatement.inScope(Return(exp))._2
      }
    )+")"
  }
}
class Func0Lit[+R <: JsAny](f: () => JsExp[R]) extends FuncXLit[R, JsFunction0[R]](0) {
  lazy val (exp, statements) = JsStatement.inScope { f() }
}
class Func1Lit[-P <: JsAny, +R <: JsAny](f: JsExp[P] => JsExp[R]) extends FuncXLit[R, P =|> R](1) {
  lazy val (exp, statements) = JsStatement.inScope {
    f(JsIdent('arg0))
  }
}
class Func2Lit[-P1 <: JsAny, -P2 <: JsAny, +R <: JsAny](f: (JsExp[P1], JsExp[P2]) => JsExp[R]) extends FuncXLit[R, JsFunction2[P1, P2, R]](2) {
  lazy val (exp, statements) = JsStatement.inScope {
    f(JsIdent('arg0), JsIdent('arg1))
  }
}
trait ToJsLow { // make sure Map has a higher priority than a regular function
  implicit def func0[R <: JsAny]: ToJsLit[() => JsExp[R], JsFunction0[R]] =
    new ToJsLit[() => JsExp[R], JsFunction0[R]]((f: () => JsExp[R]) => new Func0Lit(f))
  implicit def func1[P <: JsAny, R <: JsAny]: ToJsLit[JsExp[P] => JsExp[R], JsFunction1[P, R]] =
    new ToJsLit[JsExp[P] => JsExp[R], JsFunction1[P, R]]((f: JsExp[P] => JsExp[R]) => new Func1Lit(f))
  implicit def func2[P1 <: JsAny, P2 <: JsAny, R <: JsAny]: ToJsLit[(JsExp[P1], JsExp[P2]) => JsExp[R], JsFunction2[P1, P2, R]] =
    new ToJsLit[(JsExp[P1], JsExp[P2]) => JsExp[R], JsFunction2[P1, P2, R]]((f: (JsExp[P1], JsExp[P2]) => JsExp[R]) => new Func2Lit(f))
}
trait ToJsMedium extends ToJsLow {
  implicit val voidFunc0: ToJsLit[() => JsStatement, JsFunction0[JsVoid]] =
    new ToJsLit[() => JsStatement, JsFunction0[JsVoid]]((f: () => JsStatement) => new Func0Lit(() => { f(); JsRaw("") }))
  implicit def voidFunc1[P <: JsAny]: ToJsLit[JsExp[P] => JsStatement, P =|> JsVoid] =
    new ToJsLit[JsExp[P] => JsStatement, P =|> JsVoid]((f: JsExp[P] => JsStatement) => new Func1Lit({ x: JsExp[P] => f(x); JsRaw("") }))
  implicit def voidFunc2[P1 <: JsAny, P2 <: JsAny]: ToJsLit[(JsExp[P1], JsExp[P2]) => JsStatement, JsFunction2[P1, P2, JsVoid]] =
    new ToJsLit[(JsExp[P1], JsExp[P2]) => JsStatement, JsFunction2[P1, P2, JsVoid]](
      (f: (JsExp[P1], JsExp[P2]) => JsStatement) => new Func2Lit({ (arg0: JsExp[P1], arg1: JsExp[P2]) => f(arg0, arg1); JsRaw("") })
    )
}
/**
 * Contains implicit conversions from scala values to javascript literals.
 * Extended by object JsExp
 */
trait ToJsHigh extends ToJsMedium {
  implicit object double extends ToJsLit[Double, JsNumber]((_: Double).toString)
  implicit object int extends ToJsLit[Int, JsNumber]((_: Int).toString)
  implicit object long extends ToJsLit[Long, JsNumber]((_: Long).toString)
  implicit object bool extends ToJsLit[Boolean, JsBoolean]((_: Boolean).toString)
  implicit object string extends ToJsLit[String, JsString](net.liftweb.util.Helpers.encJs(_: String))
  implicit object date extends ToJsLit[java.util.Date, JsDate]("new Date(\""+(_: java.util.Date).toString+"\")")
  implicit object regex extends ToJsLit[scala.util.matching.Regex, JsRegex]("/"+(_: scala.util.matching.Regex).toString+"/")
  implicit object obj extends ToJsLit[Map[String, JsExp[_ <: JsAny]], JsObj]((_: Map[String, JsExp[_ <: JsAny]]).map { case (k, v) => "\""+k+"\":"+JsExp.render(v) }.mkString("{", ",", "}"))
  implicit def array[T <: JsAny]: ToJsLit[List[JsExp[T]], JsArray[T]] = new ToJsLit[List[JsExp[T]], JsArray[T]]((_: List[JsExp[T]]).map(JsExp.render).mkString("[", ",", "]"))
}

/**
 * Defines type projections as alternative syntax to construct a ToJs type.
 * ToJs.From[Int]#To[JsNumber, JsExp] == ToJs.To[JsNumber, JsExp]#From[Int] == ToJs[Int, JsNumber, JsExp]
 */
object ToJs {
  class From[S] {
    type To[J <: JsAny, E[J <: JsAny] <: JsExp[J]] = ToJs[S, J, E]
  }
  class To[J <: JsAny, E[J <: JsAny] <: JsExp[J]] {
    type From[S] = ToJs[S, J, E]
  }
}

/**
 * A typeclass to convert a `JsExp` to a scala value
 * @param encoder a scala function that provides a
 *                `JsExp[JsAny]` for the desired `JsExp`
 *                which will appear in the json encoding.
 *                For instance the default implicit instance uses `identity`.
 * @param parser a scala function that converts the lift-json `JValue` to a scala
 *               object of the desired type. For instance the default implicit instances
 *               call extract on it, with whichever Formats and manifest are
 *               pulled from the implicit scope.
 */
class FromJs[-J <: JsAny, S](val encoder: JsExp[J] => JsExp[JsAny], val parser: JsonAST.JValue => S)
object FromJs {
  implicit def fromJs[J <: JsAny, S](implicit formats: Formats = DefaultFormats, m: Manifest[S]): FromJs[J, S] = new FromJs[J, S](
    identity,
    _.extract(formats, m)
  )
  trait From[J <: JsAny] {
    type To[S] = FromJs[J, S]
  }
}

/**
 * A JsIdent whose javascript name is the scala type
 */
trait NamedIdent[T <: JsAny] extends JsIdent[T] {
  val ident = Symbol(scalaClassName(getClass))
}

object JsOp {
  def apply[L <: JsAny, R <: JsAny, T <: JsAny](l: $[L], r: $[R], op: String) = new JsOp[L, R, T](l, r, op)
}
class JsOp[-L <: JsAny, -R <: JsAny, +T <: JsAny](left: $[L], right: $[R], op: String) extends $[T] {
  def render = "("+JsExp.render(left) + op + JsExp.render(right)+")"
}

trait CanPlusLow {
  implicit def stringadd[L <: JsAny, R <: JsAny]: CanPlus[L, R, JsString] = new CanPlus((l: JsExp[L], r: JsExp[R]) => JsOp(l, r, "+"))
}
object CanPlus extends CanPlusLow {
  implicit val numNum: CanPlus[JsNumber, JsNumber, JsNumber] = new CanPlus((l: JsExp[JsNumber], r: JsExp[JsNumber]) => JsOp(l, r, "+"))
}
class CanMinus[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: (JsExp[L], JsExp[R]) => JsExp[T]) extends CanOp[L, R, T](f)
object CanMinus {
  implicit val numNum: CanMinus[JsNumber, JsNumber, JsNumber] = new CanMinus((l: JsExp[JsNumber], r: JsExp[JsNumber]) => JsOp(l, r, "-"))
}
class CanTimes[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: (JsExp[L], JsExp[R]) => JsExp[T]) extends CanOp[L, R, T](f)
object CanTimes {
  implicit val numNum: CanTimes[JsNumber, JsNumber, JsNumber] = new CanTimes((l: JsExp[JsNumber], r: JsExp[JsNumber]) => JsOp(l, r, "*"))
}
class CanDivide[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: (JsExp[L], JsExp[R]) => JsExp[T]) extends CanOp[L, R, T](f)
object CanDivide {
  implicit val numNum: CanDivide[JsNumber, JsNumber, JsNumber] = new CanDivide((l: JsExp[JsNumber], r: JsExp[JsNumber]) => JsOp(l, r, "/"))
}
class CanMod[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: (JsExp[L], JsExp[R]) => JsExp[T]) extends CanOp[L, R, T](f)
object CanMod {
  implicit val numNum: CanMod[JsNumber, JsNumber, JsNumber] = new CanMod((l: JsExp[JsNumber], r: JsExp[JsNumber]) => JsOp(l, r, "%"))
}
class CanPlus[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: (JsExp[L], JsExp[R]) => JsExp[T]) extends CanOp[L, R, T](f)

trait CanAmpLow {
  implicit def boolean[L <: JsAny, R <: JsAny] = new Can_&[L, R, JsBoolean](JsOp(_, _, "&"))
}
object Can_& extends CanAmpLow {
  implicit val numNum: Can_&[JsNumber, JsNumber, JsNumber] = new Can_&[JsNumber, JsNumber, JsNumber]((l, r) => JsOp(l, r, "&"))
}
class Can_&[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: ($[L], $[R]) => $[T]) extends CanOp[L, R, T](f)

trait CanBarLow {
  implicit def boolean[L <: JsAny, R <: JsAny] = new Can_|[L, R, JsBoolean](JsOp(_, _, "|"))
}
object Can_| extends CanBarLow {
  implicit val numNum: Can_|[JsNumber, JsNumber, JsNumber] = new Can_|[JsNumber, JsNumber, JsNumber]((l, r) => JsOp(l, r, "|"))
}
class Can_|[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: ($[L], $[R]) => $[T]) extends CanOp[L, R, T](f)

class CanOp[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: ($[L], $[R]) => $[T]) extends (($[L], $[R]) => $[T]) {
  def apply(left: $[L], right: $[R]) = f(left, right)
}

object CanApply {
  implicit def canApply0[R <: JsAny]: CanApply[JsFunction0[R], Unit, R] = new CanApply[JsFunction0[R], Unit, R](f => _ => Apply(f))
  implicit def canApply1[P <: JsAny, R <: JsAny]: CanApply[P =|> R, JsExp[P], R] = new CanApply[P =|> R, JsExp[P], R](f => p => Apply(f, p))
  implicit def canApply2[P1 <: JsAny, P2 <: JsAny, R <: JsAny]: CanApply[JsFunction2[P1, P2, R], (JsExp[P1], JsExp[P2]), R] =
    new CanApply[JsFunction2[P1, P2, R], (JsExp[P1], JsExp[P2]), R](f => ps => Apply(f, ps._1, ps._2))
}
class CanApply[-T <: JsAny, -P, +R <: JsAny](r: JsExp[T] => P => (JsExp[R] with JsStatement)) {
  def apply(f: JsExp[T], p: P): JsExp[R] with JsStatement = r(f)(p)
}

object CanGet {
  implicit def arrayIndex[R <: JsAny] = new CanGet[JsArray[R], JsNumber, R](a => i => new JsRaw[R](JsExp.render(a)+"["+JsExp.render(i)+"]") with Assignable[R])
  implicit def arrayKey[R <: JsAny] = new CanGet[JsArray[R], JsString, R](a => k => new JsRaw[R](JsExp.render(a)+"["+JsExp.render(k)+"]") with Assignable[R])
  implicit val objKey = new CanGet[JsObj, JsString, JsAny](o => k => new JsRaw[JsAny](JsExp.render(o)+"["+JsExp.render(k)+"]") with Assignable[JsAny])
}
class CanGet[-T <: JsAny, -I <: JsAny, R <: JsAny](r: $[T] => $[I] => Assignable[R]) {
  def apply(a: $[T], i: $[I]): Assignable[R] = r(a)(i)
}

object CanSelect {
  implicit def canSelect[T <: JsObj, T2 <: JsAny]: CanSelect[T, T2] = new CanSelect(
    o => m => JsRaw[T2](JsExp.render(o)+"."+JsExp.render(m))
  )
}
class CanSelect[-T <: JsAny, T2 <: JsAny](f: JsExp[T] => JsExp[T2] => JsExp[T2]) {
  def apply(o: JsExp[T], m: JsExp[T2]): JsExp[T2] = f(o)(m)
}

object CanOrder {
  implicit val numNum = new CanOrder[JsNumber, JsNumber, JsBoolean](op => l => r => JsOp(l, r, op))
}
class CanOrder[-L <: JsAny, -R <: JsAny, +T <: JsAny](f: String => $[L] => $[R] => $[T]) extends (String => ($[L], $[R]) => $[T]) {
  def apply(op: String) = (left: $[L], right: $[R]) => f(op)(left)(right)
}

/**
 * Traits that extends JsStub can have proxy instances vended
 * whose methods result in calls to javascript methods
 */
trait JsStub extends NamedIdent[JsObj]

/**
 * A function that converts one JsStub type to another. Preserves JsStatement stack info.
 * Use as an implicit conversion.
 * Caches in a WeakHashMap
 * @example {{{
 *   implicit object addWindowFunctions extends Extend[Window, MyWindow]
 * }}}
 */
class Extend[Old <: JsExp[_], New <: JsStub: ClassManifest] extends (Old => New) {
  val cache = new scala.collection.mutable.WeakHashMap[Old, New]

  def apply(old: Old): New = cache.getOrElseUpdate(old,
    if (!Proxy.isProxyClass(old.getClass)) jsProxy[New](old.render, Nil)
    else Proxy.getInvocationHandler(old) match {
      case sih: StubInvocationHandler[Old] =>
        jsProxy[New](sih.ident, sih.toReplace)
    }
  )
}

private[javascript] class StubInvocationHandler[T <: JsStub: ClassManifest](val ident: String, val toReplace: List[JsStatement] = Nil) extends InvocationHandler {
  def invoke(proxy: AnyRef, method: Method, args0: scala.Array[AnyRef]): AnyRef = {
    val args = args0 match { case null => scala.Array.empty case x => x }
    val clazz: Class[_] = classManifest[T].erasure

    // look for static forwarder --- that means the method has a scala method body, so invoke it
    def findAndInvokeForwarder(clazz: Class[_]): Option[Method] = try {
      Some(
        Class.
          forName(clazz.getName+"$class").
          getMethod(method.getName, clazz +: method.getParameterTypes: _*)
      )
    } catch {
      case _: NoSuchMethodException | _: ClassNotFoundException =>
        clazz.getInterfaces().map(findAndInvokeForwarder).find(_.isDefined) getOrElse (
          clazz.getSuperclass match {
            case null => None
            case c    => findAndInvokeForwarder(c)
          }
        )
    }
    //TODO hack

    findAndInvokeForwarder(clazz).map(_.invoke(null, proxy +: args: _*)) getOrElse {
      if (method.getName == "render" && method.getReturnType == classOf[String] && args.isEmpty)
        ident
      else if (method.getName == "ident" && method.getReturnType == classOf[Symbol] && args.isEmpty)
        Symbol(ident)
      else if (method.getName == "hashCode" && method.getReturnType == classOf[Int] && args.isEmpty)
        System.identityHashCode(proxy): java.lang.Integer
      else {
        //It's a field if: (1) no args, and (2) either its type is Assignable or it's a var
        val (proxy, toReplace2) =
          if (args.isEmpty && (
            classOf[Assignable[_]].isAssignableFrom(method.getReturnType()) ||
            clazz.getMethods.exists(_.getName == method.getName+"_$eq")
          )) {
            (new ProxyField(ident, method.getName), Nil)
          } else {
            val p = new ApplyProxyMethod(ident, method, args, toReplace)
            (p, p :: Nil)
          }

        // Usually just return the proxy. But if it's a JsStub then the javascript is not fully built --- we need a new proxy.for the next step.
        if (!(classOf[JsStub] isAssignableFrom method.getReturnType())) proxy
        else java.lang.reflect.Proxy.newProxyInstance(
          getClass.getClassLoader,
          method.getReturnType().getInterfaces :+ method.getReturnType(),
          new MethodInvocationHandler(proxy, toReplace2)(scala.reflect.Manifest.classType(method.getReturnType()))
        )
      }
    }
  }
}
private[javascript] class MethodInvocationHandler[A <: JsStub: ClassManifest](val apm: JsExp[_ <: JsAny], tr: List[JsStatement]) extends StubInvocationHandler(JsExp.render(apm), tr)
