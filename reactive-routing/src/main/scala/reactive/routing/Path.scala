package reactive
package routing

import scala.language.higherKinds

private class Extractor[-A, +B](f: A => Option[B]) {
  def unapply(a: A) = f(a)
}

object Path {
  type Id[A] = A
  class PFFrom[A, W[_]] {
    type T[R] = PartialFunction[A, W[R]]
  }
  /** from https://gist.github.com/milessabin/c9f8befa932d98dcc7a4
   * @author Miles Sabin
   */
  private object nsub {
    // Encoding for "A is not a subtype of B"
    trait <:!<[A, B]

    // Uses ambiguity to rule out the cases we're trying to exclude
    implicit def nsub[A, B] : A <:!< B = null
    implicit def nsubAmbig1[A, B >: A] : A <:!< B = null
    implicit def nsubAmbig2[A, B >: A] : A <:!< B = null
  }
  import nsub._

  trait PathComponentOpsBase[R <: RouteType] extends Any {
    protected def path: Path[R]
    def :/:(s: String) = PLit[R](s, path)
    def :/:[A](arg: Arg[A]) = PArg[A, R](arg, path)
  }
  trait PathRouteOpsBase[R <: RouteType] extends Any {
    def path: Path[R]
    def >>?[A](rte: R#Route[A]): PathRoute[R, A] = new PathRoute[R, A](path, rte)
    def >>[A](rte: R#Func[A])(implicit lift: FnToPF[R]): PathRoute[R, A] = new PathRoute[R, A](path, lift(rte))
  }
  trait PathParamOpsBase[R <: RouteType] extends Any {
    def path: Path[R]
    def :&:(s: String) = PLit[R](s, path)
    def :&:[A](arg: Arg[A]) = PArg[A, R](arg, path)
    def :&:[A](p: Param[A]) = PParam[A, R](p, path)
    def :&:[A](p: Params[A]) = PParams[A, R](p, path)
  }
  implicit class PathOps[R <: RouteType](val path: Path[R])(implicit nsub: Path[R] <:!< PParamBase[R]) extends PathComponentOpsBase[R]
  implicit class PathRouteOps[R <: RouteType](val path: Path[R]) extends AnyVal with PathRouteOpsBase[R]
  implicit class PParamBaseOps[R <: RouteType](val path: PParamBase[R]) extends AnyVal with PathParamOpsBase[R]
}

sealed trait HasStringable[A] {
  def stringable: Stringable[A]
  final def unapply(s: String) = stringable.parse(s)
}
class Arg[A](val stringable: Stringable[A]) extends HasStringable[A]
class Param[A](val key: String, val stringable: Stringable[A]) extends HasStringable[A]
class Params[A](val key: String, val stringable: Stringable[A]) extends HasStringable[A]

trait CanMapRoute[R <: RouteType] {
  def map[A, B](f: A => B): R#Route[A] => R#Route[B]
}
trait FnToPF[R <: RouteType] {
  type Partial[A] = R#Route[A]
  type Whole[A] = R#Func[A]
  def apply[A](v: Whole[A]): Partial[A]
}
object FnToPF {
  type >>:[A, R <: RouteType] = RouteType.PF[A, R]
  type RConst = RouteType.Const
  type X = Int >>: Int >>: RConst
  implicitly[RouteType.Const#Func[Int] =:= Int]
  implicitly[FnToPF[RConst]]
  implicitly[FnToPF[RouteType.PF[Int, RConst]]]
  fun[Int, RConst]
  implicit val const: FnToPF[RouteType.Const] = new FnToPF[RouteType.Const] {
    override def apply[A](a: A) = a
  }
  implicit def fun[In, N <: RouteType](implicit next: FnToPF[N]): FnToPF[RouteType.PF[In, N]] = new FnToPF[RouteType.PF[In, N]] {
    override def apply[R](a: In => N#Func[R]) = {
      case x => next(a(x))
    }
  }
}

sealed trait RouteType {
  type Route[R]
  type Func[R]
}
object RouteType {
  trait Const extends RouteType {
    type Route[R] = R
    type Func[R] = R
  }
  trait PF[In, N <: RouteType] extends RouteType {
    type Route[R] = PartialFunction[In, N#Route[R]]
    type Func[R] = In => N#Func[R]
  }
}
/*sealed trait Route[R] {
  type K[_]
  type W[_]
  type P[_]
  type Ret = R
  type V = K[R]
  type PV = P[R]
  type WV = W[R]
  val value: V
}
trait RouteConst[R] extends Route[R] {
  type K[T] = T
  type W[T] = T
  type P[T] = T
}
final class RoutePFK[In, N[X] <: Route[X]] private {
  type Route[R] = RoutePF[In, R] { type Next[X] = N[X] }
}
trait RoutePF[In, R] extends Route[R] {
  type Next[X] <: Route[X]
  type K[T] = PartialFunction[In, Next[R]]
  type P[T] = PartialFunction[In, Next[R]#P[T]]
  type W[T] = In => Next[R]#W[T]
}
*/

/**
 * A path is a typesafe URL template.
 * It consists of a chain of typed components,
 * (path components and query parameters)
 * which may be fixed strings, or string
 * representations of some value
 */
sealed trait Path[RT <: RouteType] {
  /**
   * The type of encoder function needed
   * for this type of `Path`
   */
  type EncodeFuncType

  def encode(location: Location): EncodeFuncType

  def construct: EncodeFuncType = encode(Location(Nil))

  def run[R](route: RT#Route[R]): PartialFunction[Location, R]

  private[routing] def downcast[S >: RT <: RouteType]: Path[S] = ??? //this
}

/**
 * Every `Path` chain ends with [[PNil]]
 * (the empty `Path`), or `PAny` ([[**]]).
 * There is only one `PNil` instance,
 * aliased as `PNil`.
 * However you don't have to write actually write `PNil`.
 */
sealed trait PNil extends Path[RouteType.Const] {
  type EncodeFuncType = Location

  def encode(l: Location) = l
  override def run[R](r: R): PartialFunction[Location, R] = {
    case loc if loc.path.isEmpty => r
  }
}

private case object PNil0 extends PNil

/**
 * Every `Path` chain ends with `PNil`,
 * or `PAny`. `PAny` represents all the
 * (remaining) url path components
 * as one `List[String]`.
 * There is only one `PAny` instance,
 * aliased as `PAny`.
 */
// TODO no reason not to use an Arg-like typesafe bijection
sealed trait PAny extends Path[RouteType.PF[List[String], RouteType.Const]] {
  type EncodeFuncType = List[String] => Location

  def encode(l: Location) = l ++ _
  override def run[R](f: PartialFunction[List[String], R]): PartialFunction[Location, R] = {
    case loc if f.isDefinedAt(loc.path) => f(loc.path)
  }
}

private case object PAny0 extends PAny

/**
 * `PLit` is a fixed-string url path component. It
 * is not converted to or from a value.
 */
case class PLit[NR <: RouteType](component: String, next: Path[NR]) extends Path[NR] {
  type EncodeFuncType = next.EncodeFuncType

  def encode(l: Location) = next.encode(l :+ component)
  override def run[R](f: NR#Route[R]): PartialFunction[Location, R] = {
    case loc @ Location(component :: _, _) if next.run(f).isDefinedAt(loc.tail) =>
      next.run(f)(loc.tail)
  }
}

/**
 * `PArg` is a url path component that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 */
case class PArg[A, NR <: RouteType](arg: Arg[A], next: Path[NR]) extends Path[RouteType.PF[A, NR]] {
  type EncodeFuncType = A => next.EncodeFuncType

  def encode(l: Location) = a => next.encode(l :+ arg.stringable.format(a))
  override def run[R](f: PartialFunction[A, NR#Route[R]]): PartialFunction[Location, R] = {
    case loc @ Location(arg(a) :: _, _) if f.isDefinedAt(a) && next.run(f(a)).isDefinedAt(loc.tail) =>
      next.run(f(a))(loc.tail)
  }
}

sealed trait PParamBase[NR <: RouteType] extends Path[NR]

/**
 * `PParam` is an optional named url query parameter that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 * The routing function receives None if the url does not contain the query parameter.
 * However if it contains it, but `param` does not parse it, then the `Path` does not match.
 */
case class PParam[A, NR <: RouteType](param: Param[A], next: Path[NR]) extends PParamBase[RouteType.PF[Option[A], NR]] {
  type EncodeFuncType = Option[A] => next.EncodeFuncType

  private[routing] val locParam = new Extractor((_: Location).takeParam(param.key))

  def encode(l: Location) = { ao =>
    val loc2 = ao match {
      case None    => l
      case Some(a) => l & ((param.key, param.stringable format a))
    }
    next.encode(loc2)
  }
  override def run[R](f: PartialFunction[Option[A], NR#Route[R]]): PartialFunction[Location, R] = {
    case locParam(param(a), loc2) if f.isDefinedAt(Some(a)) && next.run(f(Some(a))).isDefinedAt(loc2) =>
      next.run(f(Some(a)))(loc2)
    case loc if loc.query.forall(_._1 != param.key) && f.isDefinedAt(None) && next.run(f(None)).isDefinedAt(loc) =>
      next.run(f(None))(loc)
  }
}

/**
 * `PParams` is a repeatable named url query parameter, each occurence of which
 * is converted to and from a typed `List` of values. The actual conversion is provided by `arg`.
 */
case class PParams[A, NR <: RouteType](params: Params[A], next: Path[NR]) extends PParamBase[RouteType.PF[List[A], NR]] {
  type EncodeFuncType = List[A] => next.EncodeFuncType

  private[routing] val locParams = new Extractor((loc: Location) => Some(loc.takeParams(params.key)))
  private[routing] val parseAll = new Extractor((xs: List[String]) => Some(xs.map(params.stringable.parse).flatten))

  def encode(loc: Location) = as => next.encode(loc && ((params.key, as map params.stringable.format)))
  override def run[R](f: PartialFunction[List[A], NR#Route[R]]): PartialFunction[Location, R] = {
    case locParams(parseAll(as), loc2) if f.isDefinedAt(as) && next.run(f(as)).isDefinedAt(loc2) =>
      next.run(f(as))(loc2)
  }
}
