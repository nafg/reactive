package reactive

trait Subscription {
  def unsubscribe(): Unit
}

trait SimpleSubscription extends Subscription {
  protected var ref: AnyRef = null
  final def unsubscribe(): Unit = {
    cleanUp()
    ref = null
  }
  def cleanUp(): Unit
}

class SubscriptionProxy(@volatile var subscription: Subscription) extends Subscription {
  def unsubscribe() = subscription.unsubscribe
}

/**
 * Keeps a list of strong references. Used to control when observers
 * can be garbage collected. The observable uses weak references to hold
 * the observers, so that observers aren't retained in memory for the
 * entire lifetime of the observable.
 * Therefore, to make sure the observer isn't garbage collected too early, a
 * reference to it is stored in an Observing.
 * Usage: Most methods that add observers to an observable take an Observing
 * as an implicit parameter, so usually you put an implicit Observing in
 * the scope in question, and make sure it lasts as long as you need the
 * observers to last. You can do this by having the containing class
 * extends Observing (it contains an implicit pointing to itself), or
 * by writing
 * implicit val observing = new Observing {}
 * or the like. You can also pass an Observing instance explicitly to
 * any method that takes one, bypassing the implicit resolution mechanism.
 */
trait Observing {
  /**
   * Places an implicit reference to 'this' in scope
   */
  implicit val observing: Observing = this
  private val subscriptions = new AtomicRef[List[Subscription]](Nil)
  private[reactive] def addSubscription(subscription: Subscription): Unit = subscriptions.transform(subscription :: _)

  /**
   * You can write
   * [observing.] observe(signal){value => action}
   * as an alternative syntax to
   * signal.change.foreach{value => action} [(observing)]
   */
  @deprecated("Use signal.foreach", "0.4.0")
  def observe[T](s: Signal[T])(f: T => Unit) = s.change.foreach(f)(this)
  /**
   * You can write
   * [observing.] on(eventStream){event => action}
   * as an alternative syntax to
   * eventStream.change.foreach{event => action} [(observing)]
   */
  @deprecated("Use eventStream.foreach", "0.4.0")
  def on[T](e: EventSource[T])(f: T => Unit) = e.foreach(f)(this)
}

/**
 * An Observing that, rather than maintaining references itself,
 * maintains a List of Observings that all maintain all references.
 */
@deprecated("", "0.4.0")
trait ObservingGroup extends Observing {
  protected def observings: List[Observing]
  override private[reactive] def addSubscription(subscription: Subscription) = observings foreach (_ addSubscription subscription)
}
