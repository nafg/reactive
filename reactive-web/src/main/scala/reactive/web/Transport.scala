package reactive
package web

/**
 * Represents a means to transport javascript to the browser.
 * Examples include: by appending to the page document,
 * by returning it in an ajax response, and via comet.
 */
trait Transport {
  /**
   * Send some data to the browser.
   * The data may be sent immediately, or at a later time.
   */
  def queue[T: CanRender](renderable: T): Unit
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

  def queue[T](renderable: T)(implicit canRender: CanRender[T]) =
    dataRef.transform(_ :+ canRender(renderable))
}
