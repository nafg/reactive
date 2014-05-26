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
  def left(a : A) : Out
  def right(b : B) : Out
}
object Lub {
  implicit def lub[T] = new Lub[T, T, T] {
    def left(a : T) : T = a
    def right(b : T) : T = b
  }
}

trait CanMapPath[In[R <: RouteType, Y] <: Sitelet[R, Y]] {
  type Out[R <: RouteType, X] <: Sitelet[R, X]
  def apply[R1 <: RouteType, R2 <: RouteType, A, B](s: In[R1, A], f: Path[R1] => Path[R2], g: R1#Route[A] => R2#Route[B]): Out[R2, B]
}
object CanMapPath {
  class pathRoute extends CanMapPath[PathRoute] {
    type Out[R <: RouteType, Y] = PathRoute[R, Y]
    def apply[R1 <: RouteType, R2 <: RouteType, A, B](s: PathRoute[R1, A], f: Path[R1] => Path[R2], g: R1#Route[A] => R2#Route[B]) = new PathRoute[R2, B](f(s.path), g(s.route))
  }
  class mappedPathRoute[BB] extends CanMapPath[({ type T[RR <: RouteType, Y] = MappedPathRoute[RR, BB, Y] })#T] {
    type Out[R <: RouteType, Y] = PathRoute[R, Y]
    def apply[R1 <: RouteType, R2 <: RouteType, A, B](s: MappedPathRoute[R1, BB, A], f: Path[R1] => Path[R2], g: R1#Route[A] => R2#Route[B]) = new PathRoute[R2, B](f(s.path), g(s.route))
  }
  class routeSeq extends CanMapPath[RouteSeq] {
    type Out[R <: RouteType, Y] = RouteSeq[R, Y]
    def apply[R1 <: RouteType, R2 <: RouteType, A, B](s: RouteSeq[R1, A], f: Path[R1] => Path[R2], g: R1#Route[A] => R2#Route[B]) =
      new RouteSeq[R2, B](s.pathRoutes map (pr => new PathRoute(f(pr.path), g(pr.route))))
  }
  implicit def pathRoute = new pathRoute
  implicit def mappedPathRoute = new mappedPathRoute
  implicit def routeSeq = new routeSeq
}

object Sitelet {
  def empty[A, R <: RouteType]: Sitelet[R, A] = RouteSeq()
  implicit class SiteletMapPathOps[R1 <: RouteType, A, S[R <: RouteType, Y] <: Sitelet[R, Y]](s: S[R1, A]) {
    class PathMapper[R2 <: RouteType](f: Path[R1] => Path[R2]) {
      def by[B](g: R1#Route[A] => R2#Func[B])(implicit lift: FnToPF[R2], canMap: CanMapPath[S]): Sitelet[R2, B] = canMap(s, f, (ra: R1#Route[A]) => lift(g(ra)))
      def byPF[B](g: R1#Route[A] => R2#Route[B])(implicit canMap: CanMapPath[S]): Sitelet[R2, B] = canMap(s, f, g)
    }
    /**
     * Returns an intermediate helper object in order to modify a route by applying a
     * function to the paths and the routing functions.
     * @example {{{
     * val inc = "add" :/: arg[Int] >> { _ + 1 }
     * val add = inc mapPath (arg[Int] :/: _) by { f => x => y => f(x) + y }
     * }}}
     */
    def mapPath[S <: RouteType](f: Path[R1] => Path[S]): PathMapper[S] = new PathMapper[S](f)
  }
}

/**
 * A `Sitelet` can handle routes (convert locations to values)
 */
sealed trait Sitelet[R <: RouteType, A] { self =>

//  type EncodeFuncType

//  def construct: Seq[EncodeFuncType] = pathRoutes.map(_.path.construct)

  /**
   * Computes the value, if any, for the specified location.
   * The location is parsed by each path until a path is found
   * that can parse it, and then the extracted arguments are
   * passed to the corresponding routing function to compute the value returned.
   */
  def run: PartialFunction[Location, A]

  /**
   * The underlying [[PathRoute]]s
   */
  def pathRoutes: Seq[AbstractPathRoute[R, A]]

  /**
   * Appends a `PathRoute` to yield a `RouteSeq`
   */
  def &[C >: A, S <: T, T >: R <: RouteType](that: Sitelet[S, C])(implicit lub: Lub[R, S, T]): RouteSeq[T, C] = {
    //TODO are these casts safe?
//    val own = this.pathRoutes.map(_.asInstanceOf[AbstractPathRoute[R, C]])
//    new RouteSeq[R, C](own ++ that.pathRoutes.map(_.asInstanceOf[AbstractPathRoute[R, C]]))
//    new RouteSeq[T, C](this.pathRoutes.map(x => x.mapPath[T](y => y.downcast[T]).by[C](a => a)).flatMap(_.pathRoutes))
    ???
  }

  /**
   * Returns a sitelet whose value (yielded by [[run]]) is chained through
   * the provided function `f`. That is, the value yielded by the resulting sitelet,
   * by `run` for any given location, is the result of applying `f` with the
   * value yielded by the original sitelet (the left side of `map`)
   * by `run` for that same location.
   * @example {{{ "add" :/: arg[Int] >> { _ + 1 } map ("000" + _) }}}
   */
  def map[B](f: A => B): Sitelet[R, B]
}

abstract class AbstractPathRoute[R <: RouteType, A](val path: Path[R]) extends Sitelet[R, A] {
  val route: R#Route[A]
  def run = path.run(route)
  val pathRoutes = List(this)
  def map[B](f: A => B): AbstractPathRoute[R, B] = new MappedPathRoute(this, f)
}

class PathRoute[R <: RouteType, A](override val path: Path[R], val route: R#Route[A]) extends AbstractPathRoute[R, A](path)

class MappedPathRoute[R <: RouteType, A, B](val parent: AbstractPathRoute[R, A], f: A => B) extends AbstractPathRoute[R, B](parent.path) { outer =>
  val route: R#Route[B] = ??? //parent.route.map(f)
}

/**
 * A [[Sitelet]] consisting of a sequence of [[PathRoute]]s
 */
class RouteSeq[R <: RouteType, A](val pathRoutes: Seq[AbstractPathRoute[R, A]]) extends Sitelet[R, A] {
  def run = pathRoutes.foldLeft(PartialFunction.empty[Location, A])(_ orElse _.run)
  def map[B](f: A => B) = new RouteSeq(pathRoutes map (_ map f))
}

object RouteSeq {
  def apply[R <: RouteType, A](pathRoutes: AbstractPathRoute[R, A]*) = new RouteSeq(pathRoutes)
}
