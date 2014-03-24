package reactive
package routing

/**
 * This typeclass provides the ability to run
 * a path + route and produce a result.
 * Used [[Sitelet#mapPath]] and
 * to enrich [[Path]] with `>>` via [[Path.PathRouteOps]]
 */
trait CanRunPath[-P <: Path] {
  type Route[R] = P#Route[R]
  def isDefinedAt(path: P): Location => Boolean
  def run[R](path: P, route: P#Route[R]): PartialFunction[Location, R]
}
object CanRunPath {
  implicit val nil: CanRunPath[PNil] = new CanRunPath[PNil] {
    def isDefinedAt(path: PNil) = _.path.isEmpty
    def run[R](path: PNil, r: R) = {
      case loc if loc.path.isEmpty => r
    }
  }
  implicit val any: CanRunPath[PAny] = new CanRunPath[PAny] {
    def isDefinedAt(path: PAny) = _ => true
    def run[R](path: PAny, f: List[String] => R) = {
      case loc => f(loc.path)
    }
  }
  implicit def lit[N <: Path](implicit next: CanRunPath[N]): CanRunPath[PLit[N]] = new CanRunPath[PLit[N]] {
    def isDefinedAt(path: PLit[N]) = {
      case loc @ Location(path.component :: _, _) if next.isDefinedAt(path.next)(loc.tail) => true
      case _ => false
    }
    def run[R](path: PLit[N], f: N#Route[R]) = {
      case loc @ Location(path.component :: _, _) if next.isDefinedAt(path.next)(loc.tail) =>
        next.run(path.next, f)(loc.tail)
    }
  }
  implicit def arg[A, N <: Path](implicit next: CanRunPath[N]): CanRunPath[PArg[A, N]] = new CanRunPath[PArg[A, N]] {
    def isDefinedAt(path: PArg[A, N]) = {
      case loc @ Location(path.arg(a) :: _, _) if next.isDefinedAt(path.next)(loc.tail) => true
      case _ => false
    }
    def run[R](path: PArg[A, N], f: A => N#Route[R]) = {
      case loc @ Location(path.arg(a) :: _, _) if next.isDefinedAt(path.next)(loc.tail) =>
        next.run(path.next, f(a))(loc.tail)
    }
  }
  implicit def param[A, N <: Path](implicit next: CanRunPath[N]): CanRunPath[PParam[A, N]] = new CanRunPath[PParam[A, N]] {
    def isDefinedAt(path: PParam[A, N]) = {
      case path.locParam(path.param(a), loc2) if next.isDefinedAt(path.next)(loc2) => true
      case loc if loc.query.forall(_._1 != path.param.key) && next.isDefinedAt(path.next)(loc) => true
      case _ => false
    }
    override def run[R](path: PParam[A, N], f: Option[A] => N#Route[R]): PartialFunction[Location, R] = {
      case path.locParam(path.param(a), loc2) if next.isDefinedAt(path.next)(loc2) =>
        next.run(path.next, f(Some(a)))(loc2)
      case loc if loc.query.forall(_._1 != path.param.key) && next.isDefinedAt(path.next)(loc) =>
        next.run(path.next, f(None))(loc)
    }
  }
  implicit def params[A, N <: Path](implicit next: CanRunPath[N]): CanRunPath[PParams[A, N]] = new CanRunPath[PParams[A, N]] {
    def isDefinedAt(path: PParams[A, N]) = {
      case path.locParams(path.parseAll(as), loc2) if next.isDefinedAt(path.next)(loc2) => true
      case _ => false
    }
    override def run[R](path: PParams[A, N], f: List[A] => N#Route[R]): PartialFunction[Location, R] = {
      case path.locParams(path.parseAll(as), loc2) if next.isDefinedAt(path.next)(loc2) =>
        next.run(path.next, f(as))(loc2)
    }
  }
}
