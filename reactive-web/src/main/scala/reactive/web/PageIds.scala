package reactive
package web

import scala.collection.mutable
import scala.xml.{Elem, Null, UnprefixedAttribute}

import reactive.logging.Logger

/**
 * Manages a WeakHashMap of Pages to ids, so for instance
 * an RElem may render under a different id for different Pages,
 * and it can record those ids, until their Page is garbage collected.
 */
trait PageIds extends Logger {
  case class AssignedNewId(pageId: String, id: String)

  protected val pageIds = mutable.WeakHashMap[Page, String]()

  private def nextId(implicit page: Page) = {
    val ret = page.nextId
    trace(AssignedNewId(page.id, ret))
    ret
  }

  /**
   * Passes an Elem through this PageIds, either recording its id
   * for the current Page, or if it doesn't have the attribute,
   * add one from either the existing id for the Page, or if none exists,
   * generate a new one, add it to the Elem's attributes, and store it
   * in the WeakHashMap.
   * This method is called automatically, typically when rendering this object,
   * and is expected to be called once per Page.
   * Subclasses can add more new-Page-registering logic.
   */
  protected def addPage(elem: Elem)(implicit page: Page): Elem = synchronized {
    lazy val elemId = elem.attributes.get("id").map(_.text)
    val id = pageIds.getOrElseUpdate(page, elemId getOrElse nextId)
    elem % new UnprefixedAttribute("id", id, Null)
  }

  /**
   * The value of the id attribute for the Page.
   * If this is called before we have an id for the Page
   * one will be generated now, and that id will replace
   * the Elem's id in addPage if it had one.
   * On the other hand if addPage is first called with an
   * Elem that has an id, that will be returned and
   * no id will be generated.
   */
  def id(implicit page: Page): String = synchronized {
    pageIds.getOrElseUpdate(page, nextId)
  }
}
