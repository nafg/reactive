package reactive
package web

import net.liftweb.http.{ JavaScriptResponse, LiftRules, OkResponse, PostRequest, Req }
import net.liftweb.http.js.JsCmds
import net.liftweb.common.Full

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

  private[this] var shutDown = false
  private[this] val timer = new Timer(0, 60000, _ => shutDown)
  private[this] implicit object observing extends Observing
  timer foreach (_ => cleanPages())

  private[this] def cleanPages() =
    pagesSeenTime.transform(_ filter (System.currentTimeMillis - _._2 < 600000))

  private[this] object PageById {
    def unapply(id: String) =
      pagesSeenTime.get.keys.find(_.id == id) orElse pagesWeakMap.keys.find(_.id == id)
  }

  /**
   * You must call this in `boot` for [[SimpleAjaxPage]] to work.
   */
  def init(): Unit = {
    LiftRules.dispatch append {
      case req @ Req("__reactive-web-ajax" :: PageById(page) :: Nil, "", PostRequest) => () =>
        addPage(page)
        req.json map (json => JavaScriptResponse(JsCmds.Run(page.handleAjax(json).mkString(";\n"))))
      case req @ Req("__reactive-web-ajax" :: PageById(page) :: "heartbeat" :: Nil, "", PostRequest) => () =>
        addPage(page)
        Full(OkResponse())
    }
    LiftRules.unloadHooks append { () => shutDown = true }
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
        this.heartBeatTimeout = window.setTimeout(function() {{ reactive.doHeartBeat() }}, 120000);
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
