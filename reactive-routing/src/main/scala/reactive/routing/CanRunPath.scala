package reactive
package routing

/**
 * This typeclass provides the ability to run
 * a path + route and produce a result.
 * Used [[Sitelet#mapPath]] and
 * to enrich [[Path]] with `>>` via [[Path.PathRouteOps]]
 */
trait CanRunPath[RT <: RouteType, P <: Path[RT]] {
  type PartialRoute[R] = RT#Route[R]
  type Route[R] = RT#Func[R]
  def run[R](path: P, route: PartialRoute[R]): PartialFunction[Location, R]
//  def wholeToPartial[R](whole: Route[R]): PartialRoute[R]
}
object CanRunPath {
  import RouteType._
  implicit val nil: CanRunPath[Const, PNil] = new CanRunPath[Const, PNil] {
    def run[R](path: PNil, r: R) = {
      case loc if loc.path.isEmpty => r
    }
    def wholeToPartial[R](w: R) = w
  }
  implicit val any: CanRunPath[PF[List[String], Const], PAny] = new CanRunPath[PF[List[String], Const], PAny] {
    def run[R](path: PAny, f: PartialRoute[R]) = {
      case loc if f.isDefinedAt(loc.path) => f(loc.path)
    }
    def wholeToPartial[R](w: List[String] => R) = PartialFunction(w)
  }
  implicit def lit[NR <: RouteType, N <: Path[NR]](implicit next: CanRunPath[NR, N]): CanRunPath[NR, PLit[NR, N]] = new CanRunPath[NR, PLit[NR, N]] {
    def run[R](path: PLit[NR, N], f: PartialRoute[R]) = {
      case loc @ Location(path.component :: _, _) if next.run(path.next, f).isDefinedAt(loc.tail) =>
        next.run(path.next, f)(loc.tail)
    }
//    def wholeToPartial[R](w: N#Route[R]) = next.wholeToPartial(w)
  }
  implicit def arg[A, NR <: RouteType, N <: Path[NR]](implicit next: CanRunPath[NR, N]): CanRunPath[PF[A, NR], PArg[A, NR, N]] = new CanRunPath[PF[A, NR], PArg[A, NR, N]] {
    def run[R](path: PArg[A, NR, N], f: PartialRoute[R]) = {
      case loc @ Location(path.arg(a) :: _, _) if f.isDefinedAt(a) && next.run(path.next, f(a)).isDefinedAt(loc.tail) =>
        next.run(path.next, f(a))(loc.tail)
    }
//    def wholeToPartial[R](w: A => N#Route[R]): PartialFunction[A, N#PartialRoute[R]] = { case a => next.wholeToPartial(w(a)) }
  }
  implicit def param[A, NR <: RouteType, N <: Path[NR]](implicit next: CanRunPath[NR, N]): CanRunPath[PF[Option[A], NR], PParam[A, NR, N]] = new CanRunPath[PF[Option[A], NR], PParam[A, NR, N]] {
    override def run[R](path: PParam[A, NR, N], f: PartialRoute[R]): PartialFunction[Location, R] = {
      case path.locParam(path.param(a), loc2) if f.isDefinedAt(Some(a)) && next.run(path.next, f(Some(a))).isDefinedAt(loc2) =>
        next.run(path.next, f(Some(a)))(loc2)
      case loc if loc.query.forall(_._1 != path.param.key) && f.isDefinedAt(None) && next.run(path.next, f(None)).isDefinedAt(loc) =>
        next.run(path.next, f(None))(loc)
    }
//    def wholeToPartial[R](w: Option[A] => N#Route[R]): PartialFunction[Option[A], N#PartialRoute[R]] = { case a => next.wholeToPartial(w(a)) }
  }
  implicit def params[A, NR <: RouteType, N <: Path[NR]](implicit next: CanRunPath[NR, N]): CanRunPath[PF[List[A], NR], PParams[A, NR, N]] = new CanRunPath[PF[List[A], NR], PParams[A, NR, N]] {
    override def run[R](path: PParams[A, NR, N], f: PartialRoute[R]): PartialFunction[Location, R] = {
      case path.locParams(path.parseAll(as), loc2) if f.isDefinedAt(as) && next.run(path.next, f(as)).isDefinedAt(loc2) =>
        next.run(path.next, f(as))(loc2)
    }
//    def wholeToPartial[R](w: List[A] => N#Route[R]): PartialFunction[List[A], N#PartialRoute[R]] = { case a => next.wholeToPartial(w(a)) }
  }
}
