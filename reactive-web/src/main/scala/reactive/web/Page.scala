package reactive
package web

import net.liftweb.util.Helpers.randomString
import net.liftweb.http.{ RequestVar, S }
import net.liftweb.http.js.JsCmds
import net.liftweb.json.{ render => jrender, _ }

import scala.xml.{ Elem, UnprefixedAttribute, Null, NodeSeq }

import scala.concurrent.DelayedLazyVal

/**
 * A Page uniquely identifies a web page rendered with reactive-web components.
 * It is used to associate RElems and ReactionsComets.
 * An RElem can be associated with multiple Pages. The corresponding
 * element will be kept in sync in both places.
 */
class Page {
  /**
   * Use if you need to tie a listener's lifespan to the lifetime of the Page
   */
  implicit object observing extends Observing

  val id = randomString(20)

  @deprecated("Use id", "0.2")
  def cometName = id

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

  class AjaxTask(key: Int, events: List[JValue]) {
    @volatile var done = false
    val scope = new LocalScope
    def run = Page.withPage(Page.this) {
      Reactions.inScope(scope){
        try {
          for (event <- events)
            event match {
              case JObject(JField(jsEventStreamId, eventJson) :: Nil) =>
                try {
                  ajaxEvents.fire((jsEventStreamId, eventJson))
                } catch {
                  case e: Exception => e.printStackTrace
                }
              case _ =>
                sys.error("Invalid reactive event: " + compact(jrender(event)))
            }
        } finally {
          done = true
          completed ::= (System.currentTimeMillis(), key)
        }
      }
    }
    def js = scope.js.foldLeft(net.liftweb.http.js.JsCmds.Noop)(_ & _)
  }
  private val inProgress = new java.util.concurrent.ConcurrentHashMap[Int, AjaxTask]

  @volatile
  private var completed = List.empty[(Long, Int)]

  private val handler = S.SFuncHolder { s =>
    val (oldCompleted, recentCompleted) = completed partition (System.currentTimeMillis - _._1 > 60000)
    completed = recentCompleted
    oldCompleted foreach (inProgress remove _._2)
    parse(s) match {
      case JObject(List(JField("unique", JInt(unique)), JField("events", JArray(events)))) =>
        val key = unique.toInt
        inProgress.get(key) match {
          case null =>
            val task = new AjaxTask(key, events)
            inProgress.put(key, task)
            task.run
            task.js
          case task =>
            while (!task.done) Thread.sleep(30)
            task.js
        }

      case _ =>
        sys.error("Invalid reactive event json: " + s)
    }

  }
  Page.withPage(this) {
    Reactions.inAnyScope(this) {
      Reactions.queue(
        if (S.inStatefulScope_?) {
          S.fmapFunc(S.contextFuncBuilder(handler)){ funcId =>
            JsCmds.Run("reactive.funcId='" + funcId + "'")
          }
        } else
          JsCmds.Run("reactive.funcId='noStatefulScope'")
      )
    }
  }

  private var counter = 0

  def nextId = "reactiveWebId_%06d" format nextNumber

  def nextNumber = synchronized {
    val c = counter
    counter += 1
    c
  }

  private[web] val ajaxEvents = new EventSource[(String, JValue)] {
    override def debugName = Page.this.toString + ".ajaxEvents"
  }
}

object Page {
  /**
   * A RequestVar to generate a maximum of one Page instance
   * per request.
   */
  private object CurrentPage extends RequestVar(new Page)
  private val dynamicScope = new scala.util.DynamicVariable[Option[Page]](None)

  /**
   * Execute a block of code with a dynamically-scoped current Page
   * @param p the Page
   * @param b the block of code
   */
  def withPage[T](p: Page)(b: => T): T = dynamicScope.withValue(Some(p))(b)

  /**
   * Must be called when S.request.isDefined or there is
   * a dynamically-scoped current Page.
   */
  def currentPage: Page = {
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

  def newId = currentPageOption.map(_.nextId) getOrElse "reactiveWebId_" + randomString(7)
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
