package reactive
package web

import net.liftweb.http.js.JsCmds
import net.liftweb.http.S
import net.liftweb.json.parse

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
