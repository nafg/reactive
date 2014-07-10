package object reactive {
  implicit def ForeachableForwardable[V, S <: Foreachable[V]](self: S with Foreachable[V]): ForeachableForwardable[V, S] = new ForeachableForwardable[V, S](self)

  class ForeachableForwardable[V, S <: Foreachable[V]](val self: S) extends AnyVal with Forwardable[V, S] {
    def foreach(f: V => Unit)(implicit observing: Observing) = {
      self.foreach(f)
      self
    }
  }

  implicit def appplicative[M[_], A](ma: M[A])(implicit m: Applicative[M]): Applicative.ApplicativeBuilder[M, Applicative.ApType.One[A]] =
    new Applicative.ApplicativeBuilder.One[M, A](ma)(m)
}
