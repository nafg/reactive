package reactive
package routing

import java.time.Instant

import scala.util.Try

/**
* A typeclass for converting values to and from strings
*/
trait Stringable[A] {
  def format: A => String
  def parse: String => Option[A]
  def unapply(s: String) = parse(s)
}

object Stringable {
  implicit val string: Stringable[String] = new Stringable[String] {
    def format = identity
    def parse = Some(_)
  }
  implicit def long: Stringable[Long] = new Stringable[Long] {
    def format = _.toString
    def parse = s => Try(s.toLong).toOption
  }
  implicit val int: Stringable[Int] = new Stringable[Int] {
    def format = _.toString
    def parse = s => Try(s.toInt).toOption
  }
  implicit val bool: Stringable[Boolean] = new Stringable[Boolean] {
    def format = _.toString
    def parse = {
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
    }
  }
  implicit val instant: Stringable[Instant] = new Stringable[Instant] {
    def format = _.toString
    def parse = s => Try(Instant.parse(s)).toOption
  }
}
