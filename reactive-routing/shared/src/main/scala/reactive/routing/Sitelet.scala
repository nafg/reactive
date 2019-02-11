package reactive
package routing

import scala.language.higherKinds

/**
 * From `shapeless`.
 * Type class witnessing the least upper bound of a pair of types and providing conversions from each to their common
 * supertype.
 *
 * @author Miles Sabin
 */
trait Lub[-A, -B, +Out] {
  def left(a: A): Out
  def right(b: B): Out
}
object Lub {
  implicit def lub[T]: Lub[T, T, T] = new Lub[T, T, T] {
    def left(a: T): T = a
    def right(b: T): T = b
  }
}

object Sitelet {
  def empty[A, R <: RouteType]: Sitelet[R, A] = RouteSeq()
  implicit class SiteletOps[R1 <: RouteType, A, S[R <: RouteType, Y] <: Sitelet[R, Y]](s: S[R1, A]) {
    def &[R2 <: RouteType, R3 <: RouteType, B, C](that: Sitelet[R2, B])(implicit canAnd: CanAndSitelet[R1, R2, R3], lub: Lub[A, B, C]) = canAnd(s, that)
  }
}

/**
 * A `Sitelet` can handle routes (convert locations to values)
 */
trait Sitelet[R <: RouteType, +A] {
  type PathMapped[S <: RouteType, B] <: Sitelet[S, B]
  class PathMapper[R2 <: RouteType: CanLiftRouteMapping](f: Path[R] => Path[R2]) {
    def by[B](g: R#Route[A] => R2#Func[B])(implicit lift: FnToPF[R2]): PathMapped[R2, B] = mapPathImpl(f, ra => lift(g(ra)))
    def byPF[B](g: R#Route[A] => R2#Route[B]): PathMapped[R2, B] = mapPathImpl(f, g)
  }

  /**
   * A little DSL for modifying a `Sitelet`'s [[Path]](s).
   * @param f a function that transforms the `Path`(s) represented by this sitelet.
   * @return an intermediate helper object that has methods that take
   *         corresponding transforms of the routing function(s).
   * @example {{{
   *   val inc = "add" :/: arg[Int] >> { _ + 1 }
   *   val add = inc mapPath (arg[Int] :/: _) by { f => x => y => f(x) + y }
   *   // or
   *   inc mapPath (arg[Int] :/: _) byPF (f => { case x if x>10 => case y => f(x) + y })
   * }}}
   */
  def mapPath[S <: RouteType: CanLiftRouteMapping](f: Path[R] => Path[S]): PathMapper[S] = new PathMapper[S](f)

  /**
   * Computes the [[Location]]s for each [[PathRoute]]
   */
  def construct: Seq[R#EncodeFunc]
  /**
   * Computes the value, if any, for the specified location.
   * The location is parsed by each path until a path is found
   * that can parse it, and then the extracted arguments are
   * passed to the corresponding routing function to compute the value returned.
   */
  def run: PartialFunction[Location, A]

  /**
   * Returns a sitelet whose value (yielded by [[run]]) is chained through
   * the provided function `f`. That is, the value yielded by the resulting sitelet,
   * by `run` for any given location, is the result of applying `f` with the
   * value yielded by the original sitelet (the left side of `map`)
   * by `run` for that same location.
   * @example {{{ "add" :/: arg[Int] >> { _ + 1 } map ("000" + _) }}}
   */
  def map[B](f: A => B): Sitelet[R, B]

  /**
   * The underlying method used by [[mapPath]]
   */
  def mapPathImpl[S <: RouteType: CanLiftRouteMapping, B](f: Path[R] => Path[S], g: R#Route[A] => S#Route[B]): PathMapped[S, B]
}

/**
 * The elementary [[Sitelet]]: a pair of a [[Path]] and a [[RouteType#Route]].
 */
class PathRoute[R <: RouteType, +A](val path: Path[R], val route: R#Route[A])(implicit mapRoute: CanLiftRouteMapping[R]) extends Sitelet[R, A] {
  type PathMapped[S <: RouteType, B] = PathRoute[S, B]
  override def run = path run route
  override def construct: Seq[R#EncodeFunc] = List(path.construct)
  override def map[B](f: A => B): PathRoute[R, B] = new PathRoute(path, mapRoute(f)(route))
  override def mapPathImpl[S <: RouteType: CanLiftRouteMapping, B](f: Path[R] => Path[S], g: R#Route[A] => S#Route[B]) = new PathRoute[S, B](f(path), g(route))
}

/**
 * A [[Sitelet]] that wraps a sequence of [[PathRoute]]s
 */
class RouteSeq[R <: RouteType, +A](val pathRoutes: Seq[PathRoute[R, A]]) extends Sitelet[R, A] {
  type PathMapped[S <: RouteType, B] = RouteSeq[S, B]
  override def run = pathRoutes.foldLeft(PartialFunction.empty[Location, A])(_ orElse _.run)
  override def construct: Seq[R#EncodeFunc] = pathRoutes.map(_.path.construct)
  override def map[B](f: A => B) = new RouteSeq(pathRoutes map (_ map f))
  override def mapPathImpl[S <: RouteType: CanLiftRouteMapping, B](f: Path[R] => Path[S], g: R#Route[A] => S#Route[B]) = new RouteSeq[S, B](pathRoutes.map(_.mapPathImpl(f, g)))
}

object RouteSeq {
  def apply[R <: RouteType, A](pathRoutes: PathRoute[R, A]*) = new RouteSeq(pathRoutes)
}

/**
 * A [[Sitelet]] that acts as the composition of two sitelets.
 */
abstract class SiteletConcat[R1 <: RouteType, R2 <: RouteType, R3 <: RouteType, A, B, C](s1: Sitelet[R1, A], s2: Sitelet[R2, B])(implicit lub: Lub[A, B, C], val canAnd: CanAndSitelet[R1, R2, R3]) extends Sitelet[R3, C] {
  override def run = s1.run.andThen(lub.left) orElse s2.run.andThen(lub.right)
  override def map[D](f: C => D) = s1.map(a => f(lub.left(a))) & s2.map(b => f(lub.right(b)))
}

trait CanAndSitelet[R1 <: RouteType, R2 <: RouteType, R3 <: RouteType] {
  def apply[A, B, C](s1: Sitelet[R1, A], s2: Sitelet[R2, B])(implicit lub: Lub[A, B, C]): SiteletConcat[R1, R2, R3, A, B, C]
}
trait CanAndSiteletLow {
  implicit def other[R1 <: RouteType, R2 <: RouteType]: CanAndSitelet[R1, R2, RouteType] = new CanAndSitelet[R1, R2, RouteType] {
    def apply[A, B, C](s1: Sitelet[R1, A], s2: Sitelet[R2, B])(implicit lub: Lub[A, B, C]) =
      new SiteletConcat[R1, R2, RouteType, A, B, C](s1, s2) {
        type PathMapped[S <: RouteType, D] = Sitelet[S, D]
        override def construct = s1.construct ++ s2.construct
        override def mapPathImpl[S <: RouteType: CanLiftRouteMapping, D](f: Path[RouteType] => Path[S], g: RouteType#Route[C] => S#Route[D]) =
          s1.map(lub.left).mapPathImpl(f, g) & s2.map(lub.right).mapPathImpl(f, g)
      }
  }
}

object CanAndSitelet extends CanAndSiteletLow {
  implicit def same[R <: RouteType]: CanAndSitelet[R, R, R] = new CanAndSitelet[R, R, R] {
    def apply[A, B, C](s1: Sitelet[R, A], s2: Sitelet[R, B])(implicit lub: Lub[A, B, C]) =
      new SiteletConcat[R, R, R, A, B, C](s1, s2) {
        type PathMapped[S <: RouteType, D] = SiteletConcat[S, S, S, D, D, D]
        override def construct = s1.construct ++ s2.construct
        override def mapPathImpl[S <: RouteType: CanLiftRouteMapping, D](f: Path[R] => Path[S], g: R#Route[C] => S#Route[D]) = s1.map(lub.left).mapPathImpl(f, g) & s2.map(lub.right).mapPathImpl(f, g)
      }
  }
}
