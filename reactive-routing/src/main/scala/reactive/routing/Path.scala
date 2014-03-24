package reactive
package routing

import scala.language.higherKinds

private class Extractor[-A, +B](f: A => Option[B]) {
  def unapply(a: A) = f(a)
}

object Path {
  /** from https://gist.github.com/milessabin/c9f8befa932d98dcc7a4 */
  private object nsub {
    // Encoding for "A is not a subtype of B"
    trait <:!<[A, B]

    // Uses ambiguity to rule out the cases we're trying to exclude
    implicit def nsub[A, B] : A <:!< B = null
    implicit def nsubAmbig1[A, B >: A] : A <:!< B = null
    implicit def nsubAmbig2[A, B >: A] : A <:!< B = null
  }
  import nsub._

  implicit class PathOps[P <: Path](val path: P)(implicit nsub: P <:!< PParamBase) { ops =>
    def :/:(s: String) = PLit[P](s, path)
    def :/:[A](arg: Arg[A]) = PArg[A, P](arg, path)
  }
  implicit class PathRouteOps[P <: Path : CanRunPath](path: P) {
    def >>[A](route: P#Route[A])(implicit clrm: CanLiftRouteMapping[P]): PathRoute[P, A] = new PathRoute[P, A](path, route)
  }
  implicit class PathEncodeOps[P <: Path](path: P)(implicit protected val ep: CanEncodePath[P]) {
    def construct = ep.encode(path, Location(Nil))
  }
  implicit class PParamBaseOps[P <: PParamBase](val path: P) extends AnyVal {
    def :&:(s: String) = PLit[P](s, path)
    def :&:[A](arg: Arg[A]) = PArg[A, P](arg, path)
    def :&:[A](p: Param[A]) = PParam[A, P](p, path)
    def :&:[A](p: Params[A]) = PParams[A, P](p, path)
  }
}

sealed trait HasStringable[A] {
  def stringable: Stringable[A]
  final def unapply(s: String) = stringable.parse(s)
}
class Arg[A](val stringable: Stringable[A]) extends HasStringable[A]
class Param[A](val key: String, val stringable: Stringable[A]) extends HasStringable[A]
class Params[A](val key: String, val stringable: Stringable[A]) extends HasStringable[A]

/**
 * A path is a typesafe URL template.
 * It consists of a chain of typed components,
 * (path components and query parameters)
 * which may be fixed strings, or string
 * representations of some value
 */
sealed trait Path {
  /**
   * The kind of route needed for this type of `Path`
   */
  type Route[+R]
  /**
   * The type of encoder function needed
   * for this type of `Path`
   */
  type EncodeFuncType
}

/**
 * Every `Path` chain ends with [[PNil]]
 * (the empty `Path`), or `PAny` ([[**]]).
 * There is only one `PNil` instance,
 * aliased as `PNil`.
 * However you don't have to write actually write `PNil`.
 */
sealed trait PNil extends Path {
  type Route[+R] = R
  type EncodeFuncType = Location
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
sealed trait PAny extends Path {
  type Route[+R] = List[String] => R
  type EncodeFuncType = List[String] => Location
}

private case object PAny0 extends PAny

/**
 * `PLit` is a fixed-string url path component. It
 * is not converted to or from a value.
 */
case class PLit[Next <: Path](component: String, next: Next) extends Path {
  type Route[+R] = Next#Route[R]
  type EncodeFuncType = Next#EncodeFuncType
}

/**
 * `PArg` is a url path component that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 */
case class PArg[A, Next <: Path](arg: Arg[A], next: Next) extends Path {
  type Route[+R] = A => Next#Route[R]
  type EncodeFuncType = A => Next#EncodeFuncType
}

sealed trait PParamBase extends Path

/**
 * `PParam` is an optional named url query parameter that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 * The routing function receives None if the url does not contain the query parameter.
 * However if it contains it, but `param` does not parse it, then the `Path` does not match.
 */
case class PParam[A, Next <: Path](param: Param[A], next: Next) extends PParamBase {
  type Route[+R] = Option[A] => Next#Route[R]
  type EncodeFuncType = Option[A] => Next#EncodeFuncType

  private[routing] val locParam = new Extractor((_: Location).takeParam(param.key))
}

/**
 * `PParams` is a repeatable named url query parameter, each occurence of which
 * is converted to and from a typed `List` of values. The actual conversion is provided by `arg`.
 */
case class PParams[A, Next <: Path](params: Params[A], next: Next) extends PParamBase {
  type Route[+R] = List[A] => Next#Route[R]
  type EncodeFuncType = List[A] => Next#EncodeFuncType

  private[routing] val locParams = new Extractor((loc: Location) => Some(loc.takeParams(params.key)))
  private[routing] val parseAll = new Extractor((xs: List[String]) => Some(xs.map(params.stringable.parse).flatten))
}
