package reactive
package routing

/**
 * This typeclass allows to lift a
 * function into the PartialRoute type constructor
 * for any Path type.
 * For instance for PAny lift an `(A => B)` to a `(List[String] => A) => (List[String] => B)`.
 * Used by [[Sitelet#map]]
 */
trait CanLiftRouteMapping1[P <: Path] {
  def apply[A, B](f: A => B): P#PartialRoute[A] => P#PartialRoute[B]
}
object CanLiftRouteMapping1 {
//  implicit def all[P]: CanLiftRouteMapping[P] = new CanLiftRouteMapping[P] {
//    def apply[A, B](f: A => B): P#PartialRoute[A] => P#PartialRoute[B] = 
//  }
}
