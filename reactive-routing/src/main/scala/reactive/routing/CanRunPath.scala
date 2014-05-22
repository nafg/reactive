package reactive
package routing

/**
 * This typeclass provides the ability to run
 * a path + route and produce a result.
 * Used [[Sitelet#mapPath]] and
 * to enrich [[Path]] with `>>` via [[Path.PathRouteOps]]
 */
trait CanRunPath[P <: Path] {
  type PartialRoute[R] = P#PartialRoute[R]
  type Route[R] = P#Route[R]
  def run[R](path: P, route: P#PartialRoute[R]): PartialFunction[Location, R]
  def wholeToPartial[R](whole: Route[R]): PartialRoute[R]
}
object CanRunPath {
  implicit val nil: CanRunPath[PNil] = new CanRunPath[PNil] {
    def run[R](path: PNil, r: R) = {
      case loc if loc.path.isEmpty => r
    }
    def wholeToPartial[R](w: R) = w
  }
  implicit val any: CanRunPath[PAny] = new CanRunPath[PAny] {
    def run[R](path: PAny, f: PAny#PartialRoute[R]) = {
      case loc if f.isDefinedAt(loc.path) => f(loc.path)
    }
    def wholeToPartial[R](w: List[String] => R) = PartialFunction(w)
  }
  implicit def lit[N <: Path](implicit next: CanRunPath[N]): CanRunPath[PLit[N]] = new CanRunPath[PLit[N]] {
    def run[R](path: PLit[N], f: N#PartialRoute[R]) = {
      case loc @ Location(path.component :: _, _) if next.run(path.next, f).isDefinedAt(loc.tail) =>
        next.run(path.next, f)(loc.tail)
    }
    def wholeToPartial[R](w: N#Route[R]) = next.wholeToPartial(w)
  }
  implicit def arg[A, N <: Path](implicit next: CanRunPath[N]): CanRunPath[PArg[A, N]] = new CanRunPath[PArg[A, N]] {
    def run[R](path: PArg[A, N], f: PArg[A, N]#PartialRoute[R]) = {
      case loc @ Location(path.arg(Right(a)) :: _, _) if f.isDefinedAt(a) && next.run(path.next, f(a)).isDefinedAt(loc.tail) =>
        next.run(path.next, f(a))(loc.tail)
    }
    def wholeToPartial[R](w: A => N#Route[R]): PartialFunction[A, N#PartialRoute[R]] = { case a => next.wholeToPartial(w(a)) }
  }
  implicit def param[A, N <: Path](implicit next: CanRunPath[N]): CanRunPath[PParam[A, N]] = new CanRunPath[PParam[A, N]] {
    override def run[R](path: PParam[A, N], f: PParam[A, N]#PartialRoute[R]): PartialFunction[Location, R] = {
      case path.locParam(path.param(Right(a)), loc2) if f.isDefinedAt(Some(a)) && next.run(path.next, f(Some(a))).isDefinedAt(loc2) =>
        next.run(path.next, f(Some(a)))(loc2)
      case loc if loc.query.forall(_._1 != path.param.key) && f.isDefinedAt(None) && next.run(path.next, f(None)).isDefinedAt(loc) =>
        next.run(path.next, f(None))(loc)
    }
    def wholeToPartial[R](w: Option[A] => N#Route[R]): PartialFunction[Option[A], N#PartialRoute[R]] = { case a => next.wholeToPartial(w(a)) }
  }
  implicit def params[A, N <: Path](implicit next: CanRunPath[N]): CanRunPath[PParams[A, N]] = new CanRunPath[PParams[A, N]] {
    override def run[R](path: PParams[A, N], f: PParams[A, N]#PartialRoute[R]): PartialFunction[Location, R] = {
      case path.locParams(path.parseAll(as), loc2) if f.isDefinedAt(as.collect{case Right(a)=>a}) && next.run(path.next, f(as)).isDefinedAt(loc2) =>
        next.run(path.next, f(as))(loc2)
    }
    def wholeToPartial[R](w: List[A] => N#Route[R]): PartialFunction[List[A], N#PartialRoute[R]] = { case a => next.wholeToPartial(w(a)) }
  }
}
