package object reactive {
  implicit class ObservableForwardable[V, S <: Observable[V]](val self: S with Observable[V]) extends AnyVal with Forwardable[V, S] {
    def foreach(f: V => Unit)(implicit observing: Observing) = {
      self.foreach(f)
      self
    }
  }
}
