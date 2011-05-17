package reactive

trait CanForward[Target, Value] {
  def forward(s: Forwardable[Value], t: => Target)(implicit o: Observing)
}
object CanForward {
  implicit def thunk[T, A]: CanForward[T => A, T] = new CanForward[T => A, T] {
    def forward(s: Forwardable[T], t: => T => A)(implicit o: Observing) = s foreach { v => t(v) }
  }
  implicit def block[T, A]: CanForward[A, T] = new CanForward[A, T] {
    def forward(s: Forwardable[T], t: => A)(implicit o: Observing) = s foreach { _ => t }
  }
  implicit def vari[T]: CanForward[Var[T], T] = new CanForward[Var[T], T] {
    def forward(s: Forwardable[T], t: => Var[T])(implicit o: Observing) = s foreach t.update
  }
  implicit def eventSource[T]: CanForward[EventSource[T], T] = new CanForward[EventSource[T], T] {
    def forward(s: Forwardable[T], t: => EventSource[T])(implicit o: Observing) = s foreach t.fire
  }
}

/**
 * Something from which values can be forwarded
 */
trait Forwardable[+T] {
  def foreach(thunk: T => Unit)(implicit observing: Observing): Unit
  def >>[U >: T, S](target: => S)(implicit canForward: CanForward[S, U], observing: Observing): this.type = {
    canForward.forward(this, target)
    this
  }
  def <<:[U >: T, S](target: => S)(implicit canForward: CanForward[S, U], observing: Observing): S = {
    canForward.forward(this, target)
    target
  }
}

