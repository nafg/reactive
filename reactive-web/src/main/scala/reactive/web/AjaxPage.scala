package reactive
package web

import net.liftweb.json.{ render => jrender, _ }
import net.liftweb.http.{ JavaScriptResponse, LiftRules, OkResponse, PostRequest, Req, S }
import net.liftweb.http.js.JsCmds
import net.liftweb.common.Full

/**
 * `AjaxPage` extends [[Page]] with the ability
 * to send javascript to the browser as a response
 * to ajax requests.
 * The actual installation and linking of ajax handlers
 * is left for extending traits.
 */
trait AjaxPage extends Page {
  class AjaxTask(val key: Long, events: List[JValue]) {
    @volatile private var started = false

    @volatile var completionTime: Option[Long] = None

    object accum extends AccumulatingTransport {
      def currentPriority = 50
    }

    /**
     * Runs this task if it hasn't been run yet.
     * If `runOnce` has already been invoked, does nothing.
     */
    def runOnce() = synchronized {
      if(!started) {
        started = true
        withTransport(accum) {
          try events foreach {
            case JObject(JField(jsEventStreamId, eventJson) :: Nil) =>
              try ajaxEvents.fire((jsEventStreamId, eventJson))
              catch {
                case e: Exception => e.printStackTrace
              }
            case event =>
              sys.error("Invalid reactive event format: " + compact(jrender(event)))
          } finally {
            completionTime = Some(System.currentTimeMillis)
            notifyAll()
          }
        }
      }
    }
    /**
     * Returns the javascript after `runOnce` has completed, blocking if necessary
     */
    def take = synchronized {
      while(completionTime.isEmpty)
        wait()
      accum.data
    }
  }

  protected val tasks = new AtomicRef(List.empty[AjaxTask])

  protected def handleAjax(json: =>JValue): Seq[String] = try {
    tasks.transform(_ filter (_.completionTime map (System.currentTimeMillis - _ > 60000) getOrElse true))
    json match {
      case JObject(List(JField("unique", JInt(key)), JField("events", JArray(events)))) =>
        lazy val newTask = new AjaxTask(key.toLong, events)
        val task = tasks.run { ts =>
          ts.find(_.key == key.toLong) match {
            case Some(oldTask) => (ts,            oldTask)
            case None          => (newTask :: ts, newTask)
          }
        }
        task.runOnce()
        task.take
      case other =>
        sys.error("Invalid reactive json format: " + compact(jrender(other)))
    }
  } catch {
    case e: Exception =>
      e.printStackTrace
      Nil
  }
}

/**
 * This is the companion module for [[SimpleAjaxPage]],
 * containing the [[SimpleAjaxPage.init]] method that you must call in `boot`.
 * It also is a factory for [[SimpleAjaxPage]] instances, mixed in with [[AppendToRenderPage]].
 */
object SimpleAjaxPage {
  /**
   * @return a new [[Page]] with [[AppendToRender]] and [[SimpleAjaxPage]] functionality.
   */
  def apply() = new AppendToRenderPage with SimpleAjaxPage { }

  /**
   * This keeps a strong reference to pages, within a period of time since their last heartbeat
   */
  private[this] val pagesSeenTime = new AtomicRef(Map.empty[SimpleAjaxPage, Long])
  /**
   * This keeps a weak reference to pages, so they can outlive the heartbeat mechanism
   * in certain circumstances
   */
  private[this] val pagesWeakMap = new scala.collection.mutable.WeakHashMap[SimpleAjaxPage, Unit]

  /**
   * Adds a page, or resets its "last seen" time
   */
  private def addPage(page: SimpleAjaxPage) = {
    pagesSeenTime.transform(_ + (page -> System.currentTimeMillis))
    pagesWeakMap += page -> ()
    cleanPages()
  }

  private[this] val timer = new Timer(interval = 60000)
  private[this] implicit object observing extends Observing
  timer foreach (_ => cleanPages())

  private[this] def cleanPages() =
    pagesSeenTime.transform(_ filter (System.currentTimeMillis - _._2 < 60000))

  private[this] object PageById {
    def unapply(id: String) =
      pagesSeenTime.get.keys.find(_.id == id) orElse pagesWeakMap.keys.find(_.id == id)
  }

  /**
   * You must call this in `boot` for [[SimpleAjaxPage]] to work.
   */
  def init(): Unit = LiftRules.dispatch append {
    case req @ Req("__reactive-web-ajax" :: PageById(page) :: Nil, "", PostRequest) => () =>
      addPage(page)
      req.json map (json => JavaScriptResponse(JsCmds.Run(page.handleAjax(json).mkString(";\n"))))
    case req @ Req("__reactive-web-ajax" :: PageById(page) :: "heartbeat" :: Nil, "", PostRequest) => () =>
      addPage(page)
      Full(OkResponse())
  }
}

/**
 * This is an [[AjaxPage]] that uses a Lift dispatch
 * and plain XMLHttpRequest to install the ajax handler.
 * You must call [[AjaxPage.init]] in `boot` for it to work.
 */
trait SimpleAjaxPage extends AjaxPage {
  override def render = super.render ++
    <script type="text/javascript">
      reactive.ajaxImpl = function(url, str) {{
        var http = new XMLHttpRequest();
        http.open("POST", url);
        http.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
        http.onreadystatechange = function() {{
          if(http.readyState == 4 {"&&"} http.status == 200)
            eval(http.responseText);
        }}
        http.send(str);
      }};
      reactive.resetHeartBeat = function() {{
        if(this.heartBeatTimeout)
          window.clearTimeout(this.heartBeatTimeout);
        this.heartBeatTimeout = window.setTimeout(function() {{ reactive.doHeartBeat() }}, 12000);
      }};
      reactive.doHeartBeat = function() {{
        this.ajaxImpl("/__reactive-web-ajax/{ id }/heartbeat");
        this.resetHeartBeat();
      }};
      reactive.sendAjax = function(jsonStr) {{
        this.ajaxImpl("/__reactive-web-ajax/{ id }", jsonStr);
        this.resetHeartBeat();
      }};
      reactive.resetHeartBeat();
    </script>

  SimpleAjaxPage.addPage(this)
}

/**
 * A factory for [[LiftAjaxPage]] instances, mixed in to [[AppendToRender]]
 */
object LiftAjaxPage {
  def apply() = new AppendToRenderPage with LiftAjaxPage { }
}

/**
 * This is an [[AjaxPage]] that uses the
 * Lift function mapping mechanism and ajax code
 * to install the ajax handler.
 */
trait LiftAjaxPage extends AjaxPage {
  private val handler = ((urlParam: String) => JsCmds.Run(handleAjax(parse(urlParam)).mkString(";\n")))

  private val funcId = S.fmapFunc(S.contextFuncBuilder(S.SFuncHolder(handler)))(identity)

  override def render = super.render ++
    <script type="text/javascript">
      reactive.sendAjax = function(json) {{
        liftAjax.lift_ajaxHandler("{ funcId }=" + encodeURIComponent(json), null,
        null, null);
      }};
    </script>
}
