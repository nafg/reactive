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

  implicit class PathOps[R[_], P <: Path[R]](val path: P)(implicit nsub: P <:!< PParamBase[R]) { ops =>
    def :/:(s: String) = PLit[R](s, path)
    def :/:[A](arg: Arg[A]) = PArg[A, R](arg, path)
  }
  implicit class PathRouteOps[R[_], P <: Path[R]](val path: P) {
    def >>[A, W[_]](rte: W[A])(implicit wholeToPartial: WholeToPartial0[P, W]): PathRoute[P, A] = new PathRoute[P, A](path) { val route = wholeToPartial(rte) }
    def >>?[A](rte: R[A]): PathRoute[P, A] = new PathRoute[P, A](path) { val route = rte }
  }
  implicit class PParamBaseOps[R[_], P <: PParamBase[R]](val path: P) extends AnyVal {
    def :&:(s: String) = PLit[R](s, path)
    def :&:[A](arg: Arg[A]) = PArg[A, R](arg, path)
    def :&:[A](p: Param[A]) = PParam[A, R](p, path)
    def :&:[A](p: Params[A]) = PParams[A, R](p, path)
  }
}

sealed trait HasStringable[A] {
  def stringable: Stringable[A]
  final def unapply(s: String) = stringable.parse(s)
}
class Arg[A](val stringable: Stringable[A]) extends HasStringable[A]
class Param[A](val key: String, val stringable: Stringable[A]) extends HasStringable[A]
class Params[A](val key: String, val stringable: Stringable[A]) extends HasStringable[A]

sealed trait Route extends Any {
  type T[R]
}
trait RNil extends Route {
  type T[R] = R
}
trait RAny extends Route {
  type T[R] = R
}
trait RNil extends Route {
  type T[R] = R
}
trait RNil extends Route {
  type T[R] = R
}
trait RNil extends Route {
  type T[R] = R
}

/**
 * A path is a typesafe URL template.
 * It consists of a chain of typed components,
 * (path components and query parameters)
 * which may be fixed strings, or string
 * representations of some value
 */
sealed trait Path[+Route[_]] {
  /**
   * The type of encoder function needed
   * for this type of `Path`
   */
  type EncodeFuncType

  def encode(location: Location): EncodeFuncType

  def liftMapping[A, B](f: A => B): Route[A] => Route[B]

  def construct: EncodeFuncType = encode(Location(Nil))

  def run[R](route: Route[R]): PartialFunction[Location, R]

  private[routing] def downcast[S[X] >: Route[X]]: Path[S]
}

/**
 * Every `Path` chain ends with [[PNil]]
 * (the empty `Path`), or `PAny` ([[**]]).
 * There is only one `PNil` instance,
 * aliased as `PNil`.
 * However you don't have to write actually write `PNil`.
 */
sealed trait PNil extends Path[Path.Id] {
  type EncodeFuncType = Location

  def encode(l: Location) = l
  def liftMapping[A, B](f: A => B) = f
  def run[R](r: R) = {
    case loc if loc.path.isEmpty => r
  }

  private[routing] def downcast[S[X] >: X]: Path[S] = this
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
sealed trait PAny extends Path[PFFrom[List[String], Path.Id]#T] {
  type EncodeFuncType = List[String] => Location

  def encode(l: Location) = l ++ _
  def liftMapping[A, B](f: A => B) = _ andThen f
  def run[R](f: PartialFunction[List[String], R]) = {
    case loc if f.isDefinedAt(loc.path) => f(loc.path)
  }
}

private case object PAny0 extends PAny

/**
 * `PLit` is a fixed-string url path component. It
 * is not converted to or from a value.
 */
case class PLit[NR[_]](component: String, next: Path[NR]) extends Path[NR] {
  type EncodeFuncType = next.EncodeFuncType

  def encode(l: Location) = next.encode(l :+ component)
  def liftMapping[A, B](f: A => B) =
    pra => next.liftMapping(f)(pra)
  def run[R](f: NR[R]) = {
    case loc @ Location(component :: _, _) if next.run(f).isDefinedAt(loc.tail) =>
      next.run(f)(loc.tail)
  }
}

/**
 * `PArg` is a url path component that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 */
case class PArg[A, NR[_]](arg: Arg[A], next: Path[NR]) extends Path[PFFrom[A, NR]#T] {
  type EncodeFuncType = A => next.EncodeFuncType

  def encode(l: Location) = a => next.encode(l :+ arg.stringable.format(a))
  def liftMapping[A, B](f: A => B) = _ andThen next.liftMapping(f)
  def run[R](f: PartialFunction[A, NR[R]]) = {
    case loc @ Location(arg(a) :: _, _) if f.isDefinedAt(a) && next.run(f(a)).isDefinedAt(loc.tail) =>
      next.run(f(a))(loc.tail)
  }
}

sealed trait PParamBase[NR[_]] extends Path[NR]

/**
 * `PParam` is an optional named url query parameter that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 * The routing function receives None if the url does not contain the query parameter.
 * However if it contains it, but `param` does not parse it, then the `Path` does not match.
 */
case class PParam[A, NR[_]](param: Param[A], next: Path[NR]) extends PParamBase[PFFrom[Option[A], NR]#T] {
  type EncodeFuncType = Option[A] => next.EncodeFuncType

  private[routing] val locParam = new Extractor((_: Location).takeParam(param.key))

  def encode(l: Location) = { ao =>
    val loc2 = ao match {
      case None    => l
      case Some(a) => l & ((param.key, param.stringable format a))
    }
    next.encode(loc2)
  }
  def liftMapping[A, B](f: A => B) = _ andThen next.liftMapping(f)
  override def run[R](f: PartialFunction[Option[A], NR[R]]): PartialFunction[Location, R] = {
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
case class PParams[A, NR[_]](params: Params[A], next: Path[NR]) extends PParamBase[PFFrom[List[A], NR]#T] {
  type EncodeFuncType = List[A] => next.EncodeFuncType

  private[routing] val locParams = new Extractor((loc: Location) => Some(loc.takeParams(params.key)))
  private[routing] val parseAll = new Extractor((xs: List[String]) => Some(xs.map(params.stringable.parse).flatten))

  def encode(loc: Location) = as => next.encode(loc && ((params.key, as map params.stringable.format)))
  def liftMapping[A, B](f: A => B) = _ andThen next.liftMapping(f)
  override def run[R](f: PartialFunction[List[A], NR[R]]): PartialFunction[Location, R] = {
    case locParams(parseAll(as), loc2) if f.isDefinedAt(as) && next.run(f(as)).isDefinedAt(loc2) =>
      next.run(f(as))(loc2)
  }
}
