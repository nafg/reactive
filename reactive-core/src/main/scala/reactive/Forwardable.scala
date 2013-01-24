package reactive

/**
 * This typeclass witness the ability of a target
 * to have values forwarded to it.
 */
trait CanForwardTo[-Target, Value] {
  def forward(s: Forwardable[Value, _], t: => Target)(implicit o: Observing)
}
object CanForwardTo {
  implicit def vari[T]: CanForwardTo[Var[T], T] = new CanForwardTo[Var[T], T] {
    def forward(s: Forwardable[T, _], t: => Var[T])(implicit o: Observing) = s foreach NamedFunction(">>"+t.debugName)(t.update)
  }
  implicit def eventSource[T]: CanForwardTo[EventSource[T], T] = new CanForwardTo[EventSource[T], T] {
    def forward(s: Forwardable[T, _], t: => EventSource[T])(implicit o: Observing) = s foreach NamedFunction(">>"+t.debugString)(t.fire)
  }
}

trait Foreachable[+A] {
  def foreach(f: A => Unit)(implicit observing: Observing): Unit
}

/**
 * Something from which values can be forwarded
 */
trait Forwardable[+T, +Self] extends Any {
  def self: Self

  def foreach(thunk: T => Unit)(implicit observing: Observing): Unit

  /**
   * Forwards values from this Forwardable to a target, for whose type a CanForwardTo exists (in the implicit scope).
   * @return the forwarding instance
   */
  def >>[U >: T, S](target: => S)(implicit canForwardTo: CanForwardTo[S, U], observing: Observing): Self = {
    canForwardTo.forward(this, target)
    self
  }

  /**
   * Forwards values from this Forwardable to a target, for whose type a CanForwardTo exists (in the implicit scope).
   * This operator is available for right associativity. For example:
   * val time = Var(0) <<: timerTicks // equivalent to: val time = Var(0); timerTicks >> time
   *
   * @return the target
   */
  def <<:[U >: T, S](target: => S)(implicit canForwardTo: CanForwardTo[S, U], observing: Observing): S = {
    canForwardTo.forward(this, target)
    target
  }

  /**
   * Apply a function for every value
   */
  def =>>(thunk: T => Unit)(implicit observing: Observing): Self = {
    this foreach NamedFunction("=>>"+thunk)(thunk)
    self
  }
  /**
   * Apply a function for every value. Same as =>>.
   */
  def +=(thunk: T => Unit)(implicit observing: Observing): Self = {
    this foreach NamedFunction("+="+thunk)(thunk)
    self
  }
  /**
   * Apply a PartialFunction for every applicable value
   */
  def ?>>(pf: PartialFunction[T, Unit])(implicit observing: Observing): Self = {
    this foreach NamedFunction("?>>"+pf)((pf orElse { case _ => }))
    self
  }
  /**
   * Run a block of code for every value
   */
  def ->>(block: => Unit)(implicit observing: Observing): Self = {
    this foreach NamedFunction("->>{...}")({ _ => block })
    self
  }
}
