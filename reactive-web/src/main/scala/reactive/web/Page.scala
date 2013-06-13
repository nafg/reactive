package reactive
package web

import scala.xml.Elem
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Null
import scala.xml.UnprefixedAttribute
import net.liftweb.http.S
import net.liftweb.http.RequestVar
import net.liftweb.util.Helpers.randomString
import net.liftweb.json.JsonAST.JValue
import reactive.EventSource
import reactive.Logger
import reactive.Observing

/**
 * A Page uniquely identifies a web page rendered with reactive-web components.
 * It is used to associate RElems and ReactionsComets.
 * An RElem can be associated with multiple Pages. The corresponding
 * element will be kept in sync in both places.
 */
class Page extends Logger {
  case class ReusingScope(scope: Scope)
  case class FinishedServerScope(pageId: String, comet: ReactionsComet)
  case class QueueingJS[T: CanRender](pageId: Option[String], data: T) {
    def js: String = implicitly[CanRender[T]] apply data
  }

  /**
   * Use if you need to tie a listener's lifespan to the lifetime of the Page
   */
  implicit object observing extends Observing

  val id = randomString(20)

  lazy val comet = new ReactionsComet

  /**
   * Rendered by lift:reactive snippet invocations when Reactions.init was
   * called with comet=false (the default).
   */
  def render =
    <head>
      <script type="text/javascript" src="/classpath/reactive-web.js"/>
    </head>

  /**
   * Rendered by lift:reactive snippet invocations when Reactions.init was
   * called with comet=true
   */
  def renderComet = render ++ xml.Comment("comet " + id) ++
    <lift:comet type="net.liftweb.reactive.ReactionsComet" name={ id }/>

  private val counter = new java.util.concurrent.atomic.AtomicInteger(0)

  def nextId = "reactiveWebId_%06d" format nextNumber

  def nextNumber = counter.getAndIncrement

  private[web] val ajaxEvents = new EventSource[(String, JValue)] {
    override def debugName = Page.this.toString + ".ajaxEvents"
  }

  private[web] val _currentScope = new scala.util.DynamicVariable[Scope](DefaultScope)

  private def isOriginal = (for (r <- S.request; o <- S.originalRequest) yield r == o) openOr false

  /**
   * Executes a computation in the specified Scope.
   * @return the result of the computation
   */
  def inScope[A](scope: Scope)(block: =>A) = _currentScope.withValue(scope){ block }

  /**
   * Executes code within a "local" scope. All javascript
   * queued within the scope will be accumulated and returned.
   * @param block the code block to execute
   * @return the accumulated javascript
   */
  def inLocalScope(block: =>Unit) = {
    val scope = new LocalScope
    inScope(scope)(block)
    scope.js
  }

  /**
   * Executes code within a server scope. All javascript queued
   * within the scope will be sent to the page's comet
   * and it will be flushed to the browser.
   * @tparam A the return type of the code block
   * @param block the computation to execute
   * @return the result of the computation
   */
  def inServerScope[A](block: =>A): A = {
    val ret = _currentScope.withValue(CometScope(this)) {
      block
    }
    trace(FinishedServerScope(id, comet))
    comet.flush
    ret
  }

  /**
   * @tparam A the return type of the code block
   * @param page the Page to associate queued javascript with if there is no current scope
   * @param block the code block to execute
   * @return the result of the code block
   */
  def inAnyScope[A](block: =>A): A = {
    _currentScope.value match {
      case DefaultScope if !isOriginal =>
        Reactions.inServerScope(this)(block)
      case s =>
        trace(ReusingScope(s))
        block
    }
  }

  /**
   * Queues javascript to be rendered in the current Scope.
   */
  def queue[T: CanRender](renderable: T) = {
    trace(QueueingJS(Some(id), renderable))
    _currentScope.value queue renderable
  }

  val ajaxTransport = AjaxTransport(this)

  inAnyScope {
    implicit val _ = this
    Reactions.queue(ajaxTransport.installJs)
  }
}

object Page {
  /**
   * A RequestVar to generate a maximum of one Page instance
   * per request.
   */
  private object CurrentPage extends RequestVar(new Page)

  /**
   * For the rare case when you need to get the Page for the current Lift request.
   * Must be called when S.request.isDefined or there is
   * a dynamically-scoped current Page.
   * @throws IllegalArgumentException if called outside of a Lift request
   */
  def currentPage: Page = {
    require(S.request.isDefined, "Page.currentPage: no current request")
    CurrentPage.is
  }

  /**
   * For the rare case when you need to get the Page for the current Lift request.
   * If there is a current request, returns the value of the CurrentPage RequestVar in a Some.
   * Otherwise returns None
   */
  def currentPageOption: Option[Page] = S.request.map(_ => CurrentPage.is).toOption

}

/**
 * Manages a WeakHashMap of Pages to ids, so for instance
 * an RElem may render under a different id for different Pages,
 * and it can record those ids, until their Page is garbage collected.
 */
trait PageIds extends Logger {
  case class AssignedNewId(pageId: String, id: String)

  protected var pageIds = new scala.collection.mutable.WeakHashMap[Page, String]()

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
