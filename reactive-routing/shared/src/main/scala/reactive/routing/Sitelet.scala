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
  implicit def lub[T] = new Lub[T, T, T] {
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
  class PathMapper[R2 <: RouteType](f: Path[R] => Path[R2])(implicit mapRoute: AndThen[R2#Route]) {
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
  def mapPath[S <: RouteType](f: Path[R] => Path[S])(implicit mapRoute: AndThen[S#Route]): PathMapper[S] = new PathMapper[S](f)

  /**
   * Computes the [[Call]]s for each [[OpRoute]]
   */
  def construct: Seq[R#EncodeFunc]
  /**
   * Computes the value, if any, for the specified location.
   * The location is parsed by each path until a path is found
   * that can parse it, and then the extracted arguments are
   * passed to the corresponding routing function to compute the value returned.
   */
  def run: PartialFunction[Call, A]

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
  def mapPathImpl[S <: RouteType, B](f: Path[R] => Path[S], g: R#Route[A] => S#Route[B])(implicit mapRoute: AndThen[S#Route]): PathMapped[S, B]
}

abstract class AbstractSitelet[R <: RouteType, +A](implicit andThen: AndThen[R#Route]) extends Sitelet[R, A] {
  def map[B](f: A => B): Sitelet[R, B] = new MappedSitelet[R, A, B](this, f)
}

class MappedSitelet[R <: RouteType, A, B](val parent: Sitelet[R, A], f: A => B)(implicit andThenR: AndThen[R#Route]) extends AbstractSitelet[R, B] {
  type PathMapped[S <: RouteType, C] = parent.PathMapped[S, C]
  override def construct: Seq[R#EncodeFunc] = parent.construct
  override def run: PartialFunction[Call, B] = parent.run andThen f
  override def mapPathImpl[S <: RouteType, C](pathFunc: Path[R] => Path[S], routeFunc: R#Route[B] => S#Route[C])(implicit andThenS: AndThen[S#Route]): PathMapped[S, C] = parent.mapPathImpl[S, C](pathFunc, ra => routeFunc(andThenR(f)(ra)))
}

//class PathMappedSitelet[R <: RouteType, S <: RouteType, A, B](parent: Sitelet[R, A], f: Path[R] => Path[S], g: R#Route[A] => S#Route[B])(implicit andThenR: AndThen[R#Route], andThenS: AndThen[S#Route]) extends AbstractSitelet[S, B] {
//  override def construct: Seq[S#EncodeFunc] = parent.construct
//  override def run: PartialFunction[Location, B] = ???
//  override def mapPathImpl[S <: RouteType, B](f: (Path[S]) => Path[S], g: (S#Route[B]) => S#Route[B])(implicit mapRoute: AndThen[S#Route]): PathMapped[S, B] = ???
//}

class PathRoute[R <: RouteType, A](val path: Path[R], val route: R#Route[A])(implicit mapRoute: AndThen[R#Route]) extends OpRoute[R, A](Operation[R, A](path), route)

/**
 * The elementary [[Sitelet]]: a pair of an [[Operation]] and a [[RouteType#Route]].
 */
class OpRoute[R <: RouteType, A](val op: Operation[R, A], route: R#Route[A])(implicit mapRoute: AndThen[R#Route]) extends AbstractSitelet[R, A] {
  type PathMapped[S <: RouteType, B] = PathRoute[S, B]
  override def run = op.run(route)
  override def construct: Seq[R#EncodeFunc] = List(op.path.construct)
  override def map[B](f: A => B): OpRoute[R, B] = new OpRoute(op.copy(), mapRoute(f)(route))
  override def mapPathImpl[S <: RouteType, B](f: Path[R] => Path[S], g: R#Route[A] => S#Route[B])(implicit mapRoute: AndThen[S#Route]) = new PathRoute[S, B](f(op.path), g(route))
}

/**
 * A [[Sitelet]] that wraps a sequence of [[PathRoute]]s
 */
class RouteSeq[R <: RouteType, A](val opRoutes: Seq[OpRoute[R, A]]) extends Sitelet[R, A] {
  type PathMapped[S <: RouteType, B] = RouteSeq[S, B]
  override def run = opRoutes.foldLeft(PartialFunction.empty[Call, A])(_ orElse _.run)
  override def construct: Seq[R#EncodeFunc] = opRoutes.map(_.op.path.construct)
  override def map[B](f: A => B) = new RouteSeq(opRoutes map (_ map f))
  override def mapPathImpl[S <: RouteType, B](f: Path[R] => Path[S], g: R#Route[A] => S#Route[B])(implicit mapRoute: AndThen[S#Route]) = new RouteSeq[S, B](opRoutes.map(_.mapPathImpl(f, g)))
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
        override def mapPathImpl[S <: RouteType, D](f: Path[RouteType] => Path[S], g: RouteType#Route[C] => S#Route[D])(implicit mapRoute: AndThen[S#Route]) =
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
        override def mapPathImpl[S <: RouteType, D](f: Path[R] => Path[S], g: R#Route[C] => S#Route[D])(implicit mapRoute: AndThen[S#Route]) = s1.map(lub.left).mapPathImpl(f, g) & s2.map(lub.right).mapPathImpl(f, g)
      }
  }
}
