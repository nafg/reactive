package object reactive {
  implicit class ForeachableForwardable[V, S <: Foreachable[V]](val self: S with Foreachable[V]) extends AnyVal with Forwardable[V, S] {
    def foreach(f: V => Unit)(implicit observing: Observing) = {
      self.foreach(f)
      self
    }
  }
}
