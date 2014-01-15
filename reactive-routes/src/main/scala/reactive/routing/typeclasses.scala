package reactive
package routing


/**
 * This typeclass provides, for
 * a path, a curried function that
 * encodes values into a `Location`.
 * Used to enrich [[Path]] with `construct` via [[Path.PathEncodeOps]]
 */
trait CanEncodePath[P <: Path] {
  type EncodeFuncType = P#EncodeFuncType
  def encode(path: P, location: Location): EncodeFuncType
}
object CanEncodePath {
  implicit def nil: CanEncodePath[PNil] = new CanEncodePath[PNil] {
    def encode(p: PNil, loc: Location) = loc
  }
  implicit def any: CanEncodePath[PAny] = new CanEncodePath[PAny] {
    def encode(p: PAny, loc: Location) = loc ++ _
  }
  implicit def lit[N <: Path](implicit next: CanEncodePath[N]): CanEncodePath[PLit[N]] = new CanEncodePath[PLit[N]] {
    def encode(p: PLit[N], loc: Location) = next.encode(p.next, loc :+ p.component)
  }
implicit def arg[A, N <: Path](implicit next: CanEncodePath[N]): CanEncodePath[PArg[A, N]] = new CanEncodePath[PArg[A, N]] {
    def encode(p: PArg[A, N], loc: Location) = (a: A) => next.encode(p.next, loc :+ p.arg.stringable.format(a))
    }
implicit def param[A, N <: Path](implicit next: CanEncodePath[N]): CanEncodePath[PParam[A, N]] = new CanEncodePath[PParam[A, N]] {
    def encode(p: PParam[A, N], loc: Location) = (a: A) => next.encode(p.next, loc & (p.param.key, p.param.stringable format a))
    }
  implicit def params[A, N <: Path](implicit next: CanEncodePath[N]): CanEncodePath[PParams[A, N]] = new CanEncodePath[PParams[A, N]] {
    def encode(p: PParams[A, N], loc: Location) = (as: List[A]) => next.encode(p.next, loc && (p.params.key, as map p.params.stringable.format))
  }
}

/**
 * This typeclass allows to lift a
 * function into the Route type constructor
 * for any Path type.
 * For instance for PAny lift an `(A => B)` to a `(List[String] => A) => (List[String] => B)`.
 * Used by [[Sitelet#map]]
 */
trait CanLiftRouteMapping[P <: Path] {
  def apply[A, B](f: A => B): P#Route[A] => P#Route[B]
}
object CanLiftRouteMapping {
  implicit val nil: CanLiftRouteMapping[PNil] = new CanLiftRouteMapping[PNil] {
    def apply[A, B](f: A => B) = f
  }
  implicit val any: CanLiftRouteMapping[PAny] = new CanLiftRouteMapping[PAny] {
    def apply[A, B](f: A => B): (List[String] => A) => (List[String] => B) = _ andThen f
  }
  implicit def lit[N <: Path](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[PLit[N]] = new CanLiftRouteMapping[PLit[N]] {
    def apply[A, B](f: A => B) = next(f)
  }
  implicit def arg[A, N <: Path](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[PArg[A, N]] = new CanLiftRouteMapping[PArg[A, N]] {
    def apply[B, C](f: B => C): (A => N#Route[B]) => (A => N#Route[C]) = _ andThen next(f)
  }
  implicit def param[A, N <: Path](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[PParam[A, N]] = new CanLiftRouteMapping[PParam[A, N]] {
    def apply[B, C](f: B => C): (A => N#Route[B]) => (A => N#Route[C]) = _ andThen next(f)
  }
  implicit def params[A, N <: Path](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[PParams[A, N]] = new CanLiftRouteMapping[PParams[A, N]] {
    def apply[B, C](f: B => C): (List[A] => N#Route[B]) => (List[A] => N#Route[C]) = _ andThen next(f)
  }
}

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
      case _ => false
    }
    override def run[R](path: PParam[A, N], f: A => N#Route[R]): PartialFunction[Location, R] = {
      case path.locParam(path.param(a), loc2) if next.isDefinedAt(path.next)(loc2) =>
        next.run(path.next, f(a))(loc2)
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
