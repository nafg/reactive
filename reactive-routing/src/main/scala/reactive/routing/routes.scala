package reactive
package routing

/**
 * From `shapeless`.
 * Type class witnessing the least upper bound of a pair of types and providing conversions from each to their common
 * supertype.
 *
 * @author Miles Sabin
 */
trait Lub[-A[_], -B[_], +Out[_]] {
  def left(a : A[_]) : Out[_]
  def right(b : B[_]) : Out[_]
}
object Lub {
  implicit def lub[T[_]] = new Lub[T, T, T] {
    def left(a : T[_]) : T[_] = a
    def right(b : T[_]) : T[_] = b
  }
}

object Sitelet {
  def empty[A, R[_]]: Sitelet[R, A] = RouteSeq()
}
/**
 * A `Sitelet` can handle routes (convert locations to values)
 */
sealed trait Sitelet[+R[_], +A] { self =>
  abstract class PathMapper[S[_]](f: Path[R] => Path[S]) {
    def by[B](g: R[A] => S[B]): Sitelet[S, B]
    def byPF[B](g: R[A] => S[B]): Sitelet[S, B]
  }

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
  def &[C >: A, S[X] <: T[X], T[X] >: R[X]](that: Sitelet[S, C])(implicit lub: Lub[R, S, T]): RouteSeq[T, C] = {
    //TODO are these casts safe?
//    val own = this.pathRoutes.map(_.asInstanceOf[AbstractPathRoute[R, C]])
//    new RouteSeq[R, C](own ++ that.pathRoutes.map(_.asInstanceOf[AbstractPathRoute[R, C]]))
    new RouteSeq[T, C](this.pathRoutes.map(x => x.mapPath[T](y => y.downcast[T]).by[C](a => a)))
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
  /**
   * Returns an intermediate helper object in order to modify a route by applying a
   * function to the paths and the routing functions.
   * @example {{{
   * val inc = "add" :/: arg[Int] >> { _ + 1 }
   * val add = inc mapPath (arg[Int] :/: _) by { f => x => y => f(x) + y }
   * }}}
   */
  def mapPath[S[X] <: R[X]](f: Path[S] => Path[R]): PathMapper[R]
}

abstract class AbstractPathRoute[+R[_], +A](val path: Path[R]) extends Sitelet[R, A] {
  abstract class PathMapper[S[_]](f: Path[R] => Path[S]) extends super.PathMapper[S](f) {
    val q = f(path)
    def byPF[B](g: R[A] => S[B]): AbstractPathRoute[S, B]
  }

  val route: R[A]
  def run = path.run(route)
  val pathRoutes = List(this)
  def map[B](f: A => B): AbstractPathRoute[R, B] = new MappedPathRoute(this, f)
  override def mapPath[S[_]](f: Path[R] => Path[S]): PathMapper[S]
}

abstract class PathRoute[R[_], +A](override val path: Path[R]) extends AbstractPathRoute[R, A](path) {
  class PathMapper[S[_]](f: Path[R] => Path[S]) extends super.PathMapper[S](f) {
    def byPF[B](g: R[A] => S[B]): AbstractPathRoute[S, B] =
      new PathRoute[S, B](q) { val route: S[B] = g(PathRoute.this.route) }
  }

  val route: R[A]

  // if P#WholeRoute[A] =:= R and Q#WholeRoute[A] =:= Int => String => R, then we need a new run function
  // that whereas until now the run function took a location, compared it down the path,
  // and returned the route value, we now need a run function that parses the relevant parts
  // of the path and passes them down such a function. Pashtus we need a function P#WholeRoute[A] => Q#WholeRoute[A],
  // in the above case that would be R => (Int => String => R), which is fair enough. For instance if R is
  // case class C(x: Int = C.defaultX, y: String = C.defaultY), then the function would be
  // c => i => s => c.copy(x = i, y = s).
  // However suppose P#WholeRoute[A] =:= String => R, now how do you write (String => R) => (Int => String => R)?
  // Apparently something like f => i => s => op(f(s), i)
  // Of course there are cases that more difficult, such as (Int => String => R) => (Boolean => String => R),
  // which hopefully are not of any use anyway
  override def mapPath[S[_]](f: Path[R] => Path[S]) = new PathMapper(f)
}

class MappedPathRoute[R[_], A, B](val parent: AbstractPathRoute[R, A], f: A => B) extends AbstractPathRoute[R, B](parent.path) { outer =>
  class PathMapper[S[_]](f: Path[R] => Path[S]) extends super.PathMapper[S](f) {
    def byPF[C](g: R[B] => S[C]): PathRoute[S, C] = new PathRoute[S, C](f(parent.path)) { val route = g(outer.route) }
  }

  val route: R[B] = path.liftMapping(f)(parent.route)

  // e.g.:
  // val s: Sitelet[Int, PArg[Int, PNil], Int => ?] = (intArg >> { i => i })
  // val mapped: Sitelet[String, PArg[Int], Int => ?] = s.map(i => i.toString)
  // mapped.run(parseLocation("/10")) == "10"
  // val pathMapped: Sitelet[String, PArg[Int, PArg[Int, PNil]], Int => Int => ?] = mapped.mapPath(intArg :/: _){ f: (Int => String) => (i: Int) => (j: Int) => f(i) + " " + f(j) }
  // pathMapped.run(parseLocation("/10/20")) == "10 20"
  override def mapPath[S[_]](f: Path[R] => Path[S]) = new PathMapper(f)
}

/**
 * A [[Sitelet]] consisting of a sequence of [[PathRoute]]s
 */
class RouteSeq[R[_], A](val pathRoutes: Seq[AbstractPathRoute[R, A]]) extends Sitelet[R, A] {
  class PathMapper[S[_]](f: Path[R] => Path[S]) extends super.PathMapper[S](f) {
    def byPF[B](g: R[A] => S[B]): RouteSeq[S, B] =
      new RouteSeq(pathRoutes map (_.mapPath[S](f).byPF[B](g)))
  }

  def run = pathRoutes.foldLeft(PartialFunction.empty[Location, A])(_ orElse _.run)
  def map[B](f: A => B) = new RouteSeq(pathRoutes map (_ map f))
  override def mapPath[S[_]](f: Path[R] => Path[S]) = new PathMapper(f)
}

object RouteSeq {
  def apply[R[_], A](pathRoutes: AbstractPathRoute[R, A]*) = new RouteSeq(pathRoutes)
}
