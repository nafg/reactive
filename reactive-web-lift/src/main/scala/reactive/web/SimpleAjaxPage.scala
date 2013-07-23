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
object SimpleAjaxPage extends PagesCache[SimpleAjaxPage] {
  /**
   * @return a new [[Page]] with [[AppendToRender]] and [[SimpleAjaxPage]] functionality.
   */
  def apply() = new AppendToRenderPage with SimpleAjaxPage { }

  /**
   * This keeps a strong reference to pages, within a period of time since their last heartbeat
   */
  private[this] val pagesSeenTime = new AtomicRef(Map.empty[SimpleAjaxPage, Long])

  /**
   * Adds a page, or resets its "last seen" time
   */
  override def addPage(page: SimpleAjaxPage) = {
    super.addPage(page)
    pagesSeenTime.transform(_ + (page -> System.currentTimeMillis))
    cleanPages()
  }

  private[this] var shutDown = false
  private[this] val timer = new Timer(0, 60000, _ => shutDown)
  private[this] implicit object observing extends Observing
  timer foreach (_ => cleanPages())

  private[this] def cleanPages() =
    pagesSeenTime.transform(_ filter (System.currentTimeMillis - _._2 < 600000))

  override protected def getPage(id: String) =
    pagesSeenTime.get.keys.find(_.id == id) orElse super.getPage(id)

  /**
   * You must call this in `boot` for [[SimpleAjaxPage]] to work.
   */
  def init(): Unit = {
    LiftRules.dispatch append {
      case req @ Req("__reactive-web-ajax" :: "heartbeat" :: Nil, "", PostRequest) => () =>
        val pages = for {
          net.liftweb.json.JArray(pageIds) <- req.json.toList
          net.liftweb.json.JString(pageId) <- pageIds
        } yield getPage(pageId)
        pages foreach (_ foreach addPage)
        if(pages.forall(_.isDefined))
          Full(OkResponse())
        else
          Full(JavaScriptResponse(JsCmds.Run("alert('Some server state was lost, please reload the page.')")))
      case req @ Req("__reactive-web-ajax" :: pageId :: Nil, "", PostRequest) => () =>
        getPage(pageId) match {
          case Some(page) =>
            addPage(page)
            req.json map (json => JavaScriptResponse(JsCmds.Run(page.handleAjax(json).mkString(";\n"))))
          case None =>
            Full(JavaScriptResponse(JsCmds.Run("alert('Server state was lost, please reload the page.')")))
        }
    }
    LiftRules.unloadHooks append { () => shutDown = true }
  }
}

/**
 * This is an [[AjaxPage]] that uses a Lift dispatch
 * and plain XMLHttpRequest to install the ajax handler.
 * You must call [[SimpleAjaxPage.init]] in `boot` for it to work.
 */
trait SimpleAjaxPage extends AjaxPage {
  def heartbeatInterval = 120000

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
      if(!reactive.pageIds) reactive.pageIds = []
      reactive.pageIds.push('{ id }');
      reactive.resetHeartBeat = function() {{
        if(this.heartBeatTimeout)
          window.clearTimeout(this.heartBeatTimeout);
        this.heartBeatTimeout = window.setTimeout(function() {{ reactive.doHeartBeat() }}, { heartbeatInterval });
      }};
      reactive.doHeartBeat = function() {{
        this.ajaxImpl("/__reactive-web-ajax/heartbeat", JSON.stringify(this.pageIds));
        this.resetHeartBeat();
      }};
      reactive.sendAjax['{ id }'] = function(jsonStr) {{
        reactive.ajaxImpl("/__reactive-web-ajax/{ id }", jsonStr);
        reactive.resetHeartBeat();
      }};
      reactive.resetHeartBeat();
    </script>

  SimpleAjaxPage.addPage(this)
}
