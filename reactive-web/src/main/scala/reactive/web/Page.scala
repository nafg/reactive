package reactive
package web

import net.liftweb.util.Helpers.randomString
import net.liftweb.http.{ RequestVar, S }

import scala.xml.{ Elem, UnprefixedAttribute, Null, NodeSeq }


/**
 * A RequestVar to generate a maximum of one Page instance
 * per request.
 * The value is automatically used when an implicit Page
 * is required while a request is being processed.
 */
object CurrentPage extends RequestVar(new Page)

/**
 * A Page uniquely identifies a web page rendered with reactive-web components.
 * It is used to associate RElems and ReactionsComets.
 * An RElem can be associated with multiple Pages. The corresponding
 * element will be kept in sync in both places.
 */
class Page extends Observing {
  val id = randomString(20)
  def cometName = id
  def render = xml.Comment("comet "+id) ++
    <lift:comet type="net.liftweb.reactive.ReactionsComet" name={ cometName }/>

  private var counter = 0

  def nextId = synchronized {
    val c = counter
    counter += 1
    "reactiveWebId_%06d" format c
  }
}

object Page {
  private val dynamicScope = new scala.util.DynamicVariable[Option[Page]](None)

  /**
   * Execute a block of code with a dynamically-scoped current Page
   * @param p the Page
   * @param b the block of code
   */
  def withPage[T](p: Page)(b: => T): T = dynamicScope.withValue(Some(p))(b)

  /**
   * Makes the current Page available implicitly.
   * Must be called when S.request.isDefined or there is
   * a dynamically-scoped current Page.
   */
  implicit def currentPage: Page = {
    require(dynamicScope.value.isDefined || S.request.isDefined, "no current request, page undefined")
    dynamicScope.value getOrElse CurrentPage.is
  }

  /**
   * If there is a dynamically-scoped current Page, returns it in a Some.
   * Otherwise if there is a current request, returns the value of the CurrentPage RequestVar in a Some.
   * Otherwise returns None
   */
  def currentPageOption: Option[Page] = dynamicScope.value orElse
    S.request.map(_ => CurrentPage.is).toOption

  def newId = currentPageOption.map(_.nextId) getOrElse "reactiveWebId_"+randomString(7)
}

trait PageIds {
  protected var pageIds = new scala.collection.mutable.WeakHashMap[Page, String]()

  protected def addPage(elem: Elem)(implicit page: Page): Elem = synchronized {
    lazy val elemId = elem.attributes.get("id").map(_.text)
    val id = pageIds.getOrElseUpdate(page, elemId getOrElse Page.newId)
    elem % new UnprefixedAttribute("id", id, Null)
  }

  class Renderer(pageIds: PageIds)(renderer: Elem => Elem)(implicit page: Page) extends (NodeSeq => NodeSeq) {
    def apply(ns: NodeSeq): NodeSeq = apply(nodeSeqToElem(ns))
    def apply(elem: Elem): Elem = {
      renderer(pageIds.addPage(elem)(page))
    }
  }
}
