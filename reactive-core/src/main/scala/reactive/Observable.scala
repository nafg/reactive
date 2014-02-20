package reactive

import scala.util.{ Success, Try }

trait Observable[+A] {
  type Listener = Event[A] => Unit
  /**
   * Registers a listener function to run on each [[Event]].
   * The function is stored in a `WeakReference`,
   * and a strong reference is placed in the returned
   * [[Subscription]], so the function's garbage collectoin
   * lifetime is tied to the subscription.
   */
  def subscribe(observer: Event[A] => Unit): Subscription

  /*
   * Registers a listener function to run on each value.
   * The function is stored in a `WeakReference`,
   * and a strong reference is placed in the [[Observing]], so
   * that the function's garbage collection lifetime is tied to
   * the `Observing`'s lifetime
   * rather than the lifetime of the `Observable`.
   * @param f a function to be applied on every value
   * @param observing the object whose gc lifetime should determine that of the function
   * @return a [[Subscription]] that can be used to unsubscribe the listener early.
   */
  final def foreach(f: A => Unit)(implicit observing: Observing): Subscription = {
    val subscription = subscribe{ _.fold(())(_ foreach f) }
    observing addSubscription subscription
    subscription
  }
}

sealed trait Event[+A] extends Any {
  def fold[B](ifDone: =>B)(ifNext: Try[A] => B): B
  def foreach(f: A => Unit): Unit = fold(())(_ foreach f)
  def map[B](f: A => B): Event[B]
}

object Event {
  def apply[A](a: A) = Next(Success(a))
}

case class Next[A](value: Try[A]) extends AnyVal with Event[A] {
  def fold[B](ifDone: =>B)(ifNext: Try[A] => B) = ifNext(value)
  def map[B](f: A => B) = Next(value map f)
}
case object Done extends Event[Nothing] {
  def fold[B](ifDone: =>B)(ifNext: Try[Nothing] => B) = ifDone
  def map[B](f: Nothing => B) = Done
}
