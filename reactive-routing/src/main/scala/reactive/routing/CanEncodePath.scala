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
