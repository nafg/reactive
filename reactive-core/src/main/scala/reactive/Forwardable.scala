package reactive

trait CanForward[-Target, Value] {
  def forward(s: Forwardable[Value], t: => Target)(implicit o: Observing)
}
object CanForward {
  implicit def vari[T]: CanForward[Var[T], T] = new CanForward[Var[T], T] {
    def forward(s: Forwardable[T], t: => Var[T])(implicit o: Observing) = s forward NamedFunction(">>"+t.debugName)(t.update)
  }
  implicit def eventSource[T]: CanForward[EventSource[T], T] = new CanForward[EventSource[T], T] {
    def forward(s: Forwardable[T], t: => EventSource[T])(implicit o: Observing) = s forward NamedFunction(">>"+t.debugString)(t.fire)
  }
}

/**
 * Something from which values can be forwarded
 * Defines a number of operators that make it
 * easy to add common kinds of listeners.
 * All operators delegate to the forward method,
 * not implemented by this trait.
 */
trait Forwardable[+T] {
  def forward(f: T => Unit)(implicit o: Observing): Unit

  /**
   * Forwards values from this Forwardable to a target, for whose type a CanForward exists (in the implicit scope).
   * @return the forwarding instance
   */
  def >>[U >: T, S](target: => S)(implicit canForward: CanForward[S, U], observing: Observing): this.type = {
    canForward.forward(this, target)
    this
  }

  /**
   * Forwards values from this Forwardable to a target, for whose type a CanForward exists (in the implicit scope).
   * This operator is available for right associativity. For example:
   * val time = Var(0) <<: timerTicks // equivalent to: val time = Var(0); timerTicks >> time
   *
   * @return the target
   */
  def <<:[U >: T, S](target: => S)(implicit canForward: CanForward[S, U], observing: Observing): S = {
    canForward.forward(this, target)
    target
  }

  /**
   * Apply a function for every value
   */
  def =>>(thunk: T => Unit)(implicit observing: Observing): this.type = {
    this forward NamedFunction("=>>"+thunk)(thunk)
    this
  }
  /**
   * Apply a function for every value. Same as =>>.
   */
  def +=(thunk: T => Unit)(implicit observing: Observing): this.type = {
    this forward NamedFunction("+="+thunk)(thunk)
    this
  }
  /**
   * Apply a PartialFunction for every applicable value
   */
  def ?>>(pf: PartialFunction[T, Unit])(implicit observing: Observing): this.type = {
    this forward NamedFunction("?>>"+pf)((pf orElse { case _ => }))
    this
  }
  /**
   * Run a block of code for every value
   */
  def ->>(block: => Unit)(implicit observing: Observing): this.type = {
    this forward NamedFunction("->>{...}")({ _ => block })
    this
  }
}

