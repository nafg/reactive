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

  trait PathComponentOpsBase[R[X] <: Route[X]] extends Any {
    protected def path: Path[R]
    def :/:(s: String) = PLit[R](s, path)
    def :/:[A](arg: Arg[A]) = PArg[A, R](arg, path)
  }
  trait PathRouteOpsBase[R[X] <: Route[X]] extends Any {
    def path: Path[R]
    def >>?[A](rte: R[A]#PV)(implicit lift: CanLiftRoute[R, A, R[A]#PV]): PathRoute[R, A] = new PathRoute[R, A](path, lift(rte))
    def >>[A](rte: R[A]#WV)(implicit lift: CanLiftRoute[R, A, R[A]#WV]): PathRoute[R, A] = new PathRoute[R, A](path, lift(rte))
  }
  trait PathParamOpsBase[R[X] <: Route[X]] extends Any {
    def path: Path[R]
    def :&:(s: String) = PLit[R](s, path)
    def :&:[A](arg: Arg[A]) = PArg[A, R](arg, path)
    def :&:[A](p: Param[A]) = PParam[A, R](p, path)
    def :&:[A](p: Params[A]) = PParams[A, R](p, path)
  }
  implicit class PathOps[R[X] <: Route[X]](val path: Path[R])(implicit nsub: Path[R] <:!< PParamBase[R]) extends PathComponentOpsBase[R]
  implicit class PathRouteOps[R[X] <: Route[X]](val path: Path[R]) extends AnyVal with PathRouteOpsBase[R]
  implicit class PParamBaseOps[R[X] <: Route[X]](val path: PParamBase[R]) extends AnyVal with PathParamOpsBase[R]
}

sealed trait HasStringable[A] {
  def stringable: Stringable[A]
  final def unapply(s: String) = stringable.parse(s)
}
class Arg[A](val stringable: Stringable[A]) extends HasStringable[A]
class Param[A](val key: String, val stringable: Stringable[A]) extends HasStringable[A]
class Params[A](val key: String, val stringable: Stringable[A]) extends HasStringable[A]

trait CanMapRoute[R[X] <: Route[X]] {
  def map[A, B](f: A => B): R[A] => R[B]
}
trait CanLiftRoute[R[X] <: Route[X], A, V] {
  def apply(v: V): R[A]
}
object CanLiftRoute {
  implicit def const[A]: CanLiftRoute[RouteConst, A, A] = new CanLiftRoute[RouteConst, A, A] {
    def apply(a: A) = new RouteConst[A] { val value = a }
  }
  implicit def pf[In, R, N[R] <: Route[R]](implicit next: CanLiftRoute[N, R, N[R]#PV]) = new CanLiftRoute[RoutePFK[In, N]#Route, R, PartialFunction[In, N[R]#PV]] {
    override def apply(a: PartialFunction[In, N[R]#PV]) = new RoutePF[In, R] {
      type Next[X] = N[X]
      val value: PartialFunction[In, Next[R]] = {
        case x if a isDefinedAt x => next(a(x))
      }
    }
  }
  implicit def fun[In, R, N[R] <: Route[R]](implicit next: CanLiftRoute[N, R, N[R]#WV]) = new CanLiftRoute[RoutePFK[In, N]#Route, R, In => N[R]#WV] {
    override def apply(a: In => N[R]#WV) = new RoutePF[In, R] {
      type Next[X] = N[X]
      val value: PartialFunction[In, N[R]] = {
        case x => next(a(x))
      }
    }
  }
}
sealed trait Route[R] {
  type R
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

/**
 * A path is a typesafe URL template.
 * It consists of a chain of typed components,
 * (path components and query parameters)
 * which may be fixed strings, or string
 * representations of some value
 */
sealed trait Path[RT[X] <: Route[X]] {
  /**
   * The type of encoder function needed
   * for this type of `Path`
   */
  type EncodeFuncType

  def encode(location: Location): EncodeFuncType

  def construct: EncodeFuncType = encode(Location(Nil))

  def run[R](route: RT[R]#V): PartialFunction[Location, R]

  private[routing] def downcast[S[X] >: RT[X] <: Route[X]]: Path[S] = ??? //this
}

/**
 * Every `Path` chain ends with [[PNil]]
 * (the empty `Path`), or `PAny` ([[**]]).
 * There is only one `PNil` instance,
 * aliased as `PNil`.
 * However you don't have to write actually write `PNil`.
 */
sealed trait PNil extends Path[RouteConst] {
  type EncodeFuncType = Location

  def encode(l: Location) = l
  def run[R](r: R) = {
    case loc if loc.path.isEmpty => r
  }
}

private case object PNil0 extends PNil

import Path.PFFrom

/**
 * Every `Path` chain ends with `PNil`,
 * or `PAny`. `PAny` represents all the
 * (remaining) url path components
 * as one `List[String]`.
 * There is only one `PAny` instance,
 * aliased as `PAny`.
 */
// TODO no reason not to use an Arg-like typesafe bijection
sealed trait PAny extends Path[RoutePFK[List[String], RouteConst]#Route] {
  type EncodeFuncType = List[String] => Location

  def encode(l: Location) = l ++ _
  override def run[R](f: PartialFunction[List[String], RouteConst[R]]): PartialFunction[Location, R] = {
    case loc if f.isDefinedAt(loc.path) => f(loc.path).value
  }
}

private case object PAny0 extends PAny

/**
 * `PLit` is a fixed-string url path component. It
 * is not converted to or from a value.
 */
case class PLit[NR[X] <: Route[X]](component: String, next: Path[NR]) extends Path[NR] {
  type EncodeFuncType = next.EncodeFuncType

  def encode(l: Location) = next.encode(l :+ component)
  def run[R](f: NR[R]#V) = {
    case loc @ Location(component :: _, _) if next.run(f).isDefinedAt(loc.tail) =>
      next.run(f)(loc.tail)
  }
}

/**
 * `PArg` is a url path component that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 */
case class PArg[A, NR[X] <: Route[X]](arg: Arg[A], next: Path[NR]) extends Path[RoutePFK[A, NR]#Route] {
  type EncodeFuncType = A => next.EncodeFuncType

  def encode(l: Location) = a => next.encode(l :+ arg.stringable.format(a))
  override def run[R](f: PartialFunction[A, NR[R]]): PartialFunction[Location, R] = {
    case loc @ Location(arg(a) :: _, _) if f.isDefinedAt(a) && next.run(f(a).value).isDefinedAt(loc.tail) =>
      next.run(f(a).value)(loc.tail)
  }
}

sealed trait PParamBase[NR[X] <: Route[X]] extends Path[NR]

/**
 * `PParam` is an optional named url query parameter that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 * The routing function receives None if the url does not contain the query parameter.
 * However if it contains it, but `param` does not parse it, then the `Path` does not match.
 */
case class PParam[A, NR[X] <: Route[X]](param: Param[A], next: Path[NR]) extends PParamBase[RoutePFK[Option[A], NR]#Route] {
  type EncodeFuncType = Option[A] => next.EncodeFuncType

  private[routing] val locParam = new Extractor((_: Location).takeParam(param.key))

  def encode(l: Location) = { ao =>
    val loc2 = ao match {
      case None    => l
      case Some(a) => l & ((param.key, param.stringable format a))
    }
    next.encode(loc2)
  }
  override def run[R](f: PartialFunction[Option[A], NR[R]]): PartialFunction[Location, R] = {
    case locParam(param(a), loc2) if f.isDefinedAt(Some(a)) && next.run(f(Some(a)).value).isDefinedAt(loc2) =>
      next.run(f(Some(a)).value)(loc2)
    case loc if loc.query.forall(_._1 != param.key) && f.isDefinedAt(None) && next.run(f(None).value).isDefinedAt(loc) =>
      next.run(f(None).value)(loc)
  }
}

/**
 * `PParams` is a repeatable named url query parameter, each occurence of which
 * is converted to and from a typed `List` of values. The actual conversion is provided by `arg`.
 */
case class PParams[A, NR[X] <: Route[X]](params: Params[A], next: Path[NR]) extends PParamBase[RoutePFK[List[A], NR]#Route] {
  type EncodeFuncType = List[A] => next.EncodeFuncType

  private[routing] val locParams = new Extractor((loc: Location) => Some(loc.takeParams(params.key)))
  private[routing] val parseAll = new Extractor((xs: List[String]) => Some(xs.map(params.stringable.parse).flatten))

  def encode(loc: Location) = as => next.encode(loc && ((params.key, as map params.stringable.format)))
  override def run[R](f: PartialFunction[List[A], NR[R]]): PartialFunction[Location, R] = {
    case locParams(parseAll(as), loc2) if f.isDefinedAt(as) && next.run(f(as).value).isDefinedAt(loc2) =>
      next.run(f(as).value)(loc2)
  }
}
