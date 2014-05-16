package reactive
package web
package lift

import net.liftweb.http.{ JavaScriptResponse, LiftRules, OkResponse, PostRequest, Req }
import net.liftweb.http.js.JsCmds
import net.liftweb.common.Full
import net.liftweb.http.S

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

  def defaultLostStateHeartbeatAction = StringRenderable("alert('Some server state was lost, please reload the page.')")
  def defaultLostStateAjaxCallAction = StringRenderable("alert('Server state was lost, please reload the page.')")

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
        else {
          Full(JavaScriptResponse(JsCmds.Run("reactive.onHeartbeatLostServerState()")))
        }
      case req @ Req("__reactive-web-ajax" :: pageId :: Nil, "", PostRequest) => () =>
        getPage(pageId) match {
          case Some(page) =>
            addPage(page)
            page.transportTypes.collectFirst { case sapc: SimpleAjaxTransportType => sapc } flatMap { sapc =>
              req.json map (json => JavaScriptResponse(JsCmds.Run(sapc.handleAjax(json).map(_.render).mkString(";\n"))))
            }
          case None =>
            Full(JavaScriptResponse(JsCmds.Run("reactive.onAjaxCallLostServerState()")))
        }
    }
    LiftRules.unloadHooks append { () => shutDown = true }
  }
}

/**
 * @param beforeAjaxCall Javascript to run before sending ajax data to the server
 * @param afterAjaxCall Javascript to run after receiving the server's response
 * @param ajaxCallLostServerState Javascript to run if ajax data is sent to the server and the server reports that it has lost the page's state
 * @param heartbeatLostServerState Javascript to run if the server reports that it has lost the page's state during a heartbeat ping
 */
case class SimpleAjaxHooks(
  beforeAjaxCall: Renderable = StringRenderable(""),
  afterAjaxCall: Renderable = StringRenderable(""),
  ajaxCallLostServerState: Renderable = SimpleAjaxTransportType.defaultLostStateAjaxCallAction,
  heartbeatLostServerState: Renderable = SimpleAjaxTransportType.defaultLostStateHeartbeatAction
)

/**
 * This is an [[AjaxTransportType]] that uses a Lift dispatch
 * and plain XMLHttpRequest to install the ajax handler.
 * You must call [[SimpleAjaxTransportType.init]] in `boot` for it to work.
 * @param heartbeatInterval How often to send a heart beat. Pages with no heart beat in 10 minutes may be "forgotten" by the server
 * @param hooks Customization of javascript behavior
 */
class SimpleAjaxTransportType(page: Page, heartbeatInterval: Int = 120000, hooks: SimpleAjaxHooks = SimpleAjaxHooks()) extends AjaxTransportType {
  override def render = super.render ++
    <script type="text/javascript">
      reactive.onBeforeAjaxCall = function() {{ { hooks.beforeAjaxCall.render } }}
      reactive.onAfterAjaxCall = function() {{ { hooks.afterAjaxCall.render } }}
      reactive.onAjaxCallLostServerState = function() {{ { hooks.ajaxCallLostServerState.render } }}
      reactive.onHeartbeatLostServerState = function() {{ { hooks.heartbeatLostServerState.render } }}
      reactive.ajaxImpl = function(url, str) {{
        reactive.onBeforeAjaxCall();
        var http = new XMLHttpRequest();
        http.open("POST", url);
        http.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
        http.onreadystatechange = function() {{
          if(http.readyState == 4) {{
            reactive.onAfterAjaxCall();
            if(http.status == 200)
              eval(http.responseText);
            }}
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
        this.ajaxImpl("{ S.contextPath }/__reactive-web-ajax/heartbeat", JSON.stringify(this.pageIds));
        this.resetHeartBeat();
      }};
      reactive.sendAjax['{ page.id }'] = function(jsonStr) {{
        reactive.ajaxImpl("{ S.contextPath }/__reactive-web-ajax/{ page.id }", jsonStr);
        reactive.resetHeartBeat();
      }};
      reactive.resetHeartBeat();
    </script>

  SimpleAjaxTransportType.addPage(page)
}
