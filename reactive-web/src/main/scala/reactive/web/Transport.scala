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

  /**
   * The pages that may render to this `Transport`.
   * This may be more than one. For instance, two Lift snippets
   * may actually render to the same webpage despite defining
   * separate `Page` instances.
   */
  protected val pages = new AtomicRef(List.empty[Page])

  /**
   * Add `page` to this transport
   * Called by [[Page#linkTransport]] before it actually inserts the transport
   */
  protected[web] def addPage(page: Page): Unit = pages.transform(page :: _)

  /**
   * Remove `page` from this transport
   * Called by [[Page#unlinkTransport]] after it actually removes the transport
   */
  protected[web] def removePage(page: Page): Unit = pages.transform(_ filter (page != _))
}

trait AccumulatingTransport extends Transport {
  protected val dataRef = new AtomicRef(Vector.empty[String])

  def data = dataRef.get

  def queue[T](renderable: T)(implicit canRender: CanRender[T]) =
    dataRef.transform(_ :+ canRender(renderable))
}
