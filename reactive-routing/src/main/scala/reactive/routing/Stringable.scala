package reactive
package routing

import scala.language.higherKinds
trait Mapping[A, B] {
  type M[_]
  def format: A => B
  def parse: B => M[A]
}
/**
 * A typeclass for converting values to and from strings
 */
trait StringMapping[A] extends Mapping[A, String] {
  type Failure
  type M[X] = Either[Failure, X]
}
object StringMapping {
  implicit class fromStringable[A](s: Stringable[A]) extends StringMapping[A] {
    type Failure = Unit
    def format = s.format
    def parse = s.parse(_).toRight(())
  }

  def apply[A, Fail](parse: String => Either[Fail, A])(format: A => String): StringMapping[A] = {
    def f = format
    def p = parse
    new StringMapping[A] {
      type Failure = Fail
      def format = f
      def parse = p
    }
  }
}
trait Stringable[A] extends Mapping[A, String] {
  type M[X] = Option[X]
  def format: A => String
  def parse: String => Option[A]
}
