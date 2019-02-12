package reactive
package routing


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
  def empty[A]: Sitelet[A] = RouteSeq()

  implicit class SiteletOps[A](self: Sitelet[A]) {
    def &[B, C](that: Sitelet[B])(implicit lub: Lub[A, B, C]) = new SiteletConcat[A, B, C](self, that)
  }
}

/**
 * A `Sitelet` can handle routes (convert locations to values)
 */
trait Sitelet[+A] {
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
  def map[B](f: A => B): Sitelet[B]

}

/**
 * The elementary [[Sitelet]]: a pair of a [[Path]] and a [[RouteType#Route]].
 */
class PathRoute[R <: RouteType, +A](val path: Path[R], val route: R#Route[A])(implicit mapRoute: CanLiftRouteMapping[R]) extends Sitelet[A] {
  override def run: PartialFunction[Location, A] = path run route

  override def map[B](f: A => B): PathRoute[R, B] = new PathRoute(path, mapRoute(f)(route))
}

/**
 * A [[Sitelet]] that wraps a sequence of [[PathRoute]]s
 */
class RouteSeq[+A](val pathRoutes: Seq[PathRoute[_, A]]) extends Sitelet[A] {
  override def run: PartialFunction[Location, A] = pathRoutes.foldLeft(PartialFunction.empty[Location, A])(_ orElse _.run)

  override def map[B](f: A => B) = new RouteSeq(pathRoutes map (_ map f))
}

object RouteSeq {
  def apply[A](pathRoutes: PathRoute[_, A]*) = new RouteSeq(pathRoutes)
}

/**
 * A [[Sitelet]] that acts as the composition of two sitelets.
 */
class SiteletConcat[A, B, C](s1: Sitelet[A], s2: Sitelet[B])(implicit lub: Lub[A, B, C]) extends Sitelet[C] {
  override def run: PartialFunction[Location, C] = s1.run.andThen(lub.left) orElse s2.run.andThen(lub.right)

  override def map[D](f: C => D): SiteletConcat[D, D, D] = {
    val x: Sitelet[D] = s1.map(a => f(lub.left(a)))
    val y: Sitelet[D] = s2.map(b => f(lub.right(b)))
    x & y
  }
}
