package reactive
package routing

/**
 * This typeclass allows to lift a
 * function into the PartialRoute type constructor
 * for any Path type.
 * For instance for PAny lift an `(A => B)` to a `(List[String] => A) => (List[String] => B)`.
 * Used by [[Sitelet#map]]
 */
trait CanLiftRouteMapping[P <: Path] {
  def apply[A, B](f: A => B): P#PartialRoute[A] => P#PartialRoute[B]
}
object CanLiftRouteMapping {
  implicit val nil: CanLiftRouteMapping[PNil] = new CanLiftRouteMapping[PNil] {
    def apply[A, B](f: A => B) = f
  }
  implicit val any: CanLiftRouteMapping[PAny] = new CanLiftRouteMapping[PAny] {
    def apply[A, B](f: A => B): (PAny#PartialRoute[A]) => (PAny#PartialRoute[B]) = _ andThen f
  }
  implicit def lit[N <: Path](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[PLit[N]] = new CanLiftRouteMapping[PLit[N]] {
    def apply[A, B](f: A => B) = next(f)
  }
  implicit def arg[A, N <: Path](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[PArg[A, N]] = new CanLiftRouteMapping[PArg[A, N]] {
    def apply[B, C](f: B => C): (PArg[A, N]#PartialRoute[B]) => (PArg[A, N]#PartialRoute[C]) = _ andThen next(f)
  }
  implicit def param[A, N <: Path](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[PParam[A, N]] = new CanLiftRouteMapping[PParam[A, N]] {
    def apply[B, C](f: B => C): (PParam[A, N]#PartialRoute[B]) => (PParam[A, N]#PartialRoute[C]) = _ andThen next(f)
  }
  implicit def params[A, N <: Path](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[PParams[A, N]] = new CanLiftRouteMapping[PParams[A, N]] {
    def apply[B, C](f: B => C): (PParams[A, N]#PartialRoute[B]) => (PParams[A, N]#PartialRoute[C]) = _ andThen next(f)
  }
}
