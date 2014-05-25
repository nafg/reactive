package reactive
package routing

/**
 * This typeclass provides, for
 * a path, a curried function that
 * encodes values into a `Location`.
 * Used to enrich [[Path]] with `construct` via [[Path.PathEncodeOps]]
 */
//@deprecated("No longer necessary since Path now has an encode method", "0.4.0")
//trait CanEncodePath[P <: Path] {
//  type EncodeFuncType = P#EncodeFuncType
//  @deprecated("Use path.encode", "0.4.0")
//  def encode(path: P, location: Location): EncodeFuncType
//}
//object CanEncodePath {
//  implicit def all[P <: Path]: CanEncodePath[P] = new CanEncodePath[P] {
//    def encode(p: P, l: Location) = p encode l
//  }
//}
