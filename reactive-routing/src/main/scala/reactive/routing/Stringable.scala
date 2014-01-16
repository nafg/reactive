package reactive
package routing

/**
* A typeclass for converting values to and from strings
*/
trait Stringable[A] {
  def format: A => String
  def parse: String => Option[A]
}
