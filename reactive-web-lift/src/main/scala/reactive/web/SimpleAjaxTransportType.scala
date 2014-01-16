package reactive
package web

import net.liftweb.http.{ JavaScriptResponse, LiftRules, OkResponse, PostRequest, Req }
import net.liftweb.http.js.JsCmds
import net.liftweb.common.Full

/**
 * This is the companion module for [[SimpleAjaxTransportType]],
 * containing the [[SimpleAjaxTransportType.init]] method that you must call in `boot`.
 */
object SimpleAjaxTransportType extends PagesCache {
  /**
   * This keeps a strong reference to pages, within a period of time since their last heartbeat
   */
  private[this] val pagesSeenTime = new AtomicRef(Map.empty[Page, Long])

  /**
   * Adds a page, or resets its "last seen" time
   */
  override def addPage(page: Page) = {
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
   * You must call this in `boot` for [[SimpleAjaxTransportType]] to work.
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
            page.transportTypes.collectFirst { case sapc: SimpleAjaxTransportType => sapc } flatMap { sapc =>
              req.json map (json => JavaScriptResponse(JsCmds.Run(sapc.handleAjax(json).mkString(";\n"))))
            }
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
 * You must call [[SimpleAjaxTransportType.init]] in `boot` for it to work.
 */
class SimpleAjaxTransportType(page: Page) extends AjaxTransportType {
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
      reactive.pageIds.push('{ page.id }');
      reactive.resetHeartBeat = function() {{
        if(this.heartBeatTimeout)
          window.clearTimeout(this.heartBeatTimeout);
        this.heartBeatTimeout = window.setTimeout(function() {{ reactive.doHeartBeat() }}, { heartbeatInterval });
      }};
      reactive.doHeartBeat = function() {{
        this.ajaxImpl("/__reactive-web-ajax/heartbeat", JSON.stringify(this.pageIds));
        this.resetHeartBeat();
      }};
      reactive.sendAjax['{ page.id }'] = function(jsonStr) {{
        reactive.ajaxImpl("/__reactive-web-ajax/{ page.id }", jsonStr);
        reactive.resetHeartBeat();
      }};
      reactive.resetHeartBeat();
    </script>

  SimpleAjaxTransportType.addPage(page)
}
