package reactive
package web

/**
 * Represents a means to transport javascript to the browser.
 * Examples include: by appending to the page document,
 * by returning it in an ajax response, and via comet.
 */
trait Transport {
  protected implicit object observing extends Observing

  /**
   * Data to be sent to the browser.
   * The data may or may not be sent immediately, depending on the `Transport` implementation
   */
  lazy val queued: EventSource[Renderable] = new EventSource

  /**
   * Return a number to help the `Page`
   * select the ideal `Transport` to use.
   * `Page` will use the available `Transport`
   * with the highest priority.
   * Does not need to always return the same value.
   */
  def currentPriority: Int
}

trait AccumulatingTransport extends Transport {
  protected val dataRef = new AtomicRef(Vector.empty[Renderable])

  def data = dataRef.get

  queued foreach { x =>
    dataRef.transform(_ :+ x)
  }
}
