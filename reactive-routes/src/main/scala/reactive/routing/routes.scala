package reactive
package routing


object Sitelet {
  implicit class SiteletEncodeOps[P <: Path](s: Sitelet[P, _])(implicit val ep: CanEncodePath[P]) {
    def construct: Seq[ep.EncodeFuncType] = s.pathRoutes map { _.path.construct }
  }
  def empty[A, P <: Path]: Sitelet[P, A] = RouteSeq(Nil)
}
/**
 * A `Sitelet` can handle routes (convert locations to values)
 */
sealed trait Sitelet[P <: Path, +A] { self =>
  abstract class PathMapper[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]) {
    def by[B](g: P#Route[A] => hprQ.Route[B]): Sitelet[Q, B]
  }

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
  def pathRoutes: Seq[AbstractPathRoute[P, A]]

  /**
   * Appends a `PathRoute` to yield a `RouteSeq`
   */
  def &[C >: A, Q <: Path, R <: Path](that: AbstractPathRoute[Q, C])(implicit lub: Lub[P,Q,R]): RouteSeq[R, C] = {
    //TODO are these casts safe?
    val own = this.pathRoutes.map(_.asInstanceOf[AbstractPathRoute[R, C]])
    RouteSeq[R, C](own :+ that.asInstanceOf[AbstractPathRoute[R, C]])
  }

  /**
   * Returns a sitelet whose value (yielded by [[run]]) is chained through
   * the provided function `f`. That is, the value yielded by the resulting sitelet,
   * by `run` for any given location, is the result of applying `f` with the
   * value yielded by the original sitelet (the left side of `map`)
   * by `run` for that same location.
   * @example {{{ "add" :/: arg[Int] >> { _ + 1 } map ("000" + _) }}}
   */
  def map[B](f: A => B)(implicit cmr: CanLiftRouteMapping[P]): Sitelet[P, B]
  /**
   * Returns an intermediate helper object in order to modify a route by applying a
   * function to the paths and the routing functions.
   * @example {{{
   * val inc = "add" :/: arg[Int] >> { _ + 1 }
   * val add = inc mapPath (arg[Int] :/: _) by { f => x => y => f(x) + y }
   * }}}
   */
  def mapPath[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]): PathMapper[Q]
}

abstract class AbstractPathRoute[P <: Path, +A](val path: P)(implicit val hpr: CanRunPath[P]) extends Sitelet[P, A] {
  abstract class PathMapper[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]) extends super.PathMapper[Q](f) {
    def by[B](g: P#Route[A] => hprQ.Route[B]): AbstractPathRoute[Q, B]
  }

  def route: P#Route[A]
  def run = hpr.run(path, route)
  val pathRoutes = List(this)
  def map[B](f: A => B)(implicit cmr: CanLiftRouteMapping[P]): AbstractPathRoute[P, B] = new MappedPathRoute(this, f)
  override def mapPath[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]): PathMapper[Q]
}

class PathRoute[P <: Path, +A](path: P, val route: P#Route[A])(implicit hpr: CanRunPath[P]) extends AbstractPathRoute[P, A](path) {
  class PathMapper[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]) extends super.PathMapper[Q](f) {
    def by[B](g: P#Route[A] => hprQ.Route[B]): PathRoute[Q, B] = new PathRoute[Q, B](f(path), g(route))
  }
  // if P#Route[A] =:= R and Q#Route[A] =:= Int => String => R, then we need a new run function
  // that whereas until now the run function took a location, compared it down the path,
  // and returned the route value, we now need a run function that parses the relevant parts
  // of the path and passes them down such a function. Pashtus we need a function P#Route[A] => Q#Route[A],
  // in the above case that would be R => (Int => String => R), which is fair enough. For instance if R is
  // case class C(x: Int = C.defaultX, y: String = C.defaultY), then the function would be
  // c => i => s => c.copy(x = i, y = s).
  // However suppose P#Route[A] =:= String => R, now how do you write (String => R) => (Int => String => R)?
  // Apparently something like f => i => s => op(f(s), i)
  // Of course there are cases that more difficult, such as (Int => String => R) => (Boolean => String => R),
  // which hopefully are not of any use anyway
  override def mapPath[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]) = new PathMapper(f)
}
class MappedPathRoute[P <: Path, A, B](parent: AbstractPathRoute[P, A], f: A => B)(implicit cmr: CanLiftRouteMapping[P], hpr: CanRunPath[P]) extends AbstractPathRoute[P, B](parent.path) { outer =>
  class PathMapper[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]) extends super.PathMapper[Q](f) {
    def by[C](g: P#Route[B] => hprQ.Route[C]): PathRoute[Q, C] = new PathRoute[Q, C](f(parent.path), g(outer.route))
  }

  def route: P#Route[B] = cmr(f)(parent.route)

  // e.g.:
  // val s: Sitelet[Int, PArg[Int, PNil], Int => ?] = (intArg >> { i => i })
  // val mapped: Sitelet[String, PArg[Int], Int => ?] = s.map(i => i.toString)
  // mapped.run(parseLocation("/10")) == "10"
  // val pathMapped: Sitelet[String, PArg[Int, PArg[Int, PNil]], Int => Int => ?] = mapped.mapPath(intArg :/: _){ f: (Int => String) => (i: Int) => (j: Int) => f(i) + " " + f(j) }
  // pathMapped.run(parseLocation("/10/20")) == "10 20"
  override def mapPath[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]) = new PathMapper(f)
}
case class RouteSeq[P <: Path, A](pathRoutes: Seq[AbstractPathRoute[P, A]]) extends Sitelet[P, A] {
  class PathMapper[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]) extends super.PathMapper[Q](f) {
    def by[B](g: P#Route[A] => hprQ.Route[B]): RouteSeq[Q, B] = RouteSeq(pathRoutes map (_.mapPath[Q](f).by[B](g)))
  }

  def run = pathRoutes.foldLeft(PartialFunction.empty[Location, A])(_ orElse _.run)
  def map[B](f: A => B)(implicit cmr: CanLiftRouteMapping[P]) = RouteSeq(pathRoutes map (_ map f))
  override def mapPath[Q <: Path](f: P => Q)(implicit hprQ: CanRunPath[Q]) = new PathMapper(f)
}
