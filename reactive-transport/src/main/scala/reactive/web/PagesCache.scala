package reactive
package web

trait PagesCache {
  /**
   * This keeps a weak reference to pages, so they can outlive the heartbeat mechanism
   * in certain circumstances
   */
  private val pagesWeakMap = new scala.collection.mutable.WeakHashMap[Page, Unit]

  /**
   * Adds a page
   */
  def addPage(page: Page): Unit = pagesWeakMap += page -> ()

  private def keys = pagesWeakMap.synchronized { pagesWeakMap.keySet.toSeq }

  protected def getPage(id: String) = keys.toList.find(_.id == id)

  /**
   * An extractor to locate a `Page` by its id
   */
  protected object PageById {
    final def unapply(id: String) = getPage(id)
  }
}
