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
  def left(a : A) : Out
  def right(b : B) : Out
}

object Lub {
  implicit def lub[T] = new Lub[T, T, T] {
    def left(a : T) : T = a
    def right(b : T) : T = b
  }
}
