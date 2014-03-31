package object reactive {
  implicit def ForeachableForwardable[V, S <: Foreachable[V]](self: S with Foreachable[V]): ForeachableForwardable[V, S] = new ForeachableForwardable[V, S](self)

  class ForeachableForwardable[V, S <: Foreachable[V]](val self: S) extends AnyVal with Forwardable[V, S] {
    def foreach(f: V => Unit)(implicit observing: Observing) = {
      self.foreach(f)
      self
    }
  }
}
