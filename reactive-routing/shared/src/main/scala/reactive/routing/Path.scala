package reactive
package routing

private class Extractor[-A, +B](f: A => Option[B]) {
  def unapply(a: A) = f(a)
}

/**
 * Provides the DSL, in conjuction with the package object
 * @example {{{
 *   "lit" :/: arg[Int] :&: param[String]("q") >> { i => s => (i, s) }
 *   // is equivalent to
 *   new PathRoute(PLit("lit", PArg(arg[Int], PParam[String]("q"))), { case i => case s => (i, s) })
 * }}}
 * @note You only have to import reactive.routing._ -- the implicits here are found
 *       automatically since scala looks in the companion object.
 */
object Path {
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

  class PathRouteOpsBase[R <: RouteType](path: Path[R]) {
    def >>?[A](rte: R#Route[A])(implicit mapRoute: CanLiftRouteMapping[R]): PathRoute[R, A] = new PathRoute[R, A](path, rte)
    def >>[A](rte: R#Func[A])(implicit mapRoute: CanLiftRouteMapping[R], lift: FnToPF[R]): PathRoute[R, A] = new PathRoute[R, A](path, lift(rte))
  }
  class PathComponentOpsBase[R <: RouteType, P <: Path[R]](path: P with Path[R]) extends PathRouteOpsBase[R](path) {
    def :/:(s: String) = PLit[R, P](s, path)
    def :/:[A](arg: Arg[A]) = PArg[A, R, P](arg, path)
  }
  class PathParamOpsBase[R <: RouteType, P <: Path[R]](path: P with Path[R]) extends PathRouteOpsBase[R](path) {
    def :&:(s: String) = PLit[R, P](s, path)
    def :&:[A](arg: Arg[A]) = PArg[A, R, P](arg, path)
    def :&:[A](p: Param[A]) = PParam[A, R, P](p, path)
    def :&:[A](p: Params[A]) = PParams[A, R, P](p, path)
  }
  implicit class PathOps[R <: RouteType, P <: Path[R]](val path: P with Path[R])(implicit nsub: P <:!< PParamBase[R]) extends PathComponentOpsBase[R, P](path)
  implicit class PParamBaseOps[R <: RouteType, P <: PParamBase[R]](val path: P with Path[R]) extends PathParamOpsBase[R, P](path)

  /**
   * part of a workaround that allows [[Path]] to be covariant
   */
  implicit class PathIsRunnable[R <: RouteType](path: Path[R]) {
    def run[A](route: R#Route[A]): PartialFunction[Location, A] = path match {
      case p: RunnablePath[R] => p.run[A](route)
    }
  }
}

class Arg[A](val stringable: Stringable[A])
class Param[A](val key: String, val stringable: Stringable[A])
class Params[A](val key: String, val stringable: Stringable[A])

/**
 * A path is a typesafe URL template.
 * It consists of a chain of typed components,
 * (path components and query parameters)
 * which may be fixed strings, or string
 * representations of some value
 * @tparam RT the [[RouteType]] that determines the structure of the route.
 * It depends directly on the actual `Path` chain.
 */
sealed trait Path[+RT <: RouteType] { this: RunnablePath[_ <: RT] =>
  protected[routing] def encode(location: Location): RT#EncodeFunc
  /**
   * Returns a [[Location]], or a curried function returning a Location,
   * depending on the [[RouteType]]
   */
  def construct: RT#EncodeFunc = encode(Location(Nil))
}

/**
 * part of a workaround that allows [[Path]] to be covariant
 */
sealed trait RunnablePath[RT <: RouteType] extends Path[RT] {
  /**
   * Given a route value, returns a `PartialFunction` that
   * parses a [[Location]] and passes the parameters
   * to the route value, if applicable
   */
  def run[R](route: RT#Route[R]): PartialFunction[Location, R]
}

/**
 * Every [[Path]] chain ends with `PNil`
 * (the empty `Path`), or [[PAny]] ([[**]]).
 * There is only one `PNil` instance,
 * aliased as `PNil`.
 * However the DSL does not require you to actually write `PNil`.
 */
sealed trait PNil extends RunnablePath[RConst] {
  override def encode(l: Location): Location = l
  override def run[R](r: R): PartialFunction[Location, R] = {
    case loc if loc.path.isEmpty => r
  }
}

private case object PNil0 extends PNil

/**
 * Every [[Path]] chain ends with [[PNil]],
 * or `PAny`. `PAny` represents all the
 * (remaining) url path components
 * as one `List[String]`.
 * There is only one `PAny` instance,
 * aliased as `PAny`.
 */
// TODO no reason not to use an Arg-like typesafe bijection
sealed trait PAny extends RunnablePath[RFunc[List[String], RConst]] {
  override def encode(l: Location): List[String] => Location = l ++ _
  override def run[R](f: PartialFunction[List[String], R]): PartialFunction[Location, R] = {
    case loc if f.isDefinedAt(loc.path) => f(loc.path)
  }
}

private case object PAny0 extends PAny

/**
 * `PLit` is a fixed-string url path component. It
 * is not converted to or from a value.
 */
case class PLit[NR <: RouteType, N <: Path[NR]](component: String, next: N with Path[NR]) extends RunnablePath[NR] {
  override def encode(l: Location): NR#EncodeFunc = next.encode(l :+ component)
  override def run[R](f: NR#Route[R]): PartialFunction[Location, R] = {
    case loc @ Location(`component` :: _, _) if next.run(f).isDefinedAt(loc.tail) =>
      next.run(f)(loc.tail)
  }
}

/**
 * `PArg` is a url path component that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 */
case class PArg[A, NR <: RouteType, N <: Path[NR]](arg: Arg[A], next: N) extends RunnablePath[RFunc[A, NR]] {
  override def encode(l: Location): A => NR#EncodeFunc = a => next.encode(l :+ arg.stringable.format(a))
  override def run[R](f: PartialFunction[A, NR#Route[R]]): PartialFunction[Location, R] = {
    case loc @ Location(arg.stringable(a) :: _, _) if f.isDefinedAt(a) && next.run(f(a)).isDefinedAt(loc.tail) =>
      next.run(f(a))(loc.tail)
  }
}

/**
 * Marker trait, used by the DSL so that `:&:` is used rather than `:/:`
 */
sealed trait PParamBase[NR <: RouteType] extends RunnablePath[NR]

/**
 * `PParam` is an optional named url query parameter that is converted to and
 * from a typed value. The actual conversion is provided by `arg`.
 * The routing function receives None if the url does not contain the query parameter.
 * However if it contains it, but `param` does not parse it, then the `Path` does not match.
 */
case class PParam[A, NR <: RouteType, N <: Path[NR]](param: Param[A], next: N) extends PParamBase[RFunc[Option[A], NR]] {
  private val locParam = new Extractor((_: Location).takeParam(param.key))

  override def encode(l: Location): Option[A] => NR#EncodeFunc = { ao =>
    val loc2 = ao match {
      case None    => l
      case Some(a) => l & ((param.key, param.stringable format a))
    }
    next.encode(loc2)
  }
  override def run[R](f: PartialFunction[Option[A], NR#Route[R]]): PartialFunction[Location, R] = {
    case locParam(param.stringable(a), loc2) if f.isDefinedAt(Some(a)) && next.run(f(Some(a))).isDefinedAt(loc2) =>
      next.run(f(Some(a)))(loc2)
    case loc if loc.query.forall(_._1 != param.key) && f.isDefinedAt(None) && next.run(f(None)).isDefinedAt(loc) =>
      next.run(f(None))(loc)
  }
}

/**
 * `PParams` is a repeatable named url query parameter, each occurence of which
 * is converted to and from a typed `List` of values. The actual conversion is provided by `arg`.
 */
case class PParams[A, NR <: RouteType, N <: Path[NR]](params: Params[A], next: N) extends PParamBase[RFunc[List[A], NR]] {
  private val locParams = new Extractor((loc: Location) => Some(loc.takeParams(params.key)))
  private val parseAll = new Extractor((xs: List[String]) => Some(xs.map(params.stringable.parse).flatten))

  override def encode(loc: Location): List[A] => NR#EncodeFunc = as => next.encode(loc && ((params.key, as map params.stringable.format)))
  override def run[R](f: PartialFunction[List[A], NR#Route[R]]): PartialFunction[Location, R] = {
    case locParams(parseAll(as), loc2) if f.isDefinedAt(as) && next.run(f(as)).isDefinedAt(loc2) =>
      next.run(f(as))(loc2)
  }
}
