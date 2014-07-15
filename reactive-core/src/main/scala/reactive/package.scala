import scala.language.higherKinds

package object reactive {
  implicit class ForeachableForwardable[V, S <: Foreachable[V]](val _s: S with Foreachable[V]) extends AnyVal with Forwardable[V, S] {
    def self = _s
    def foreach(f: V => Unit)(implicit observing: Observing) = {
      self.foreach(f)
      self
    }
  }

  implicit class appplicative[M[_], A](ma: M[A])(implicit m: Applicative[M]) extends Applicative.ApplicativeBuilder.One[M, A](ma)(m)
}
