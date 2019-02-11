package reactive
package web
package lift

import net.liftweb.http.S
import net.liftweb.http.js.JsCmd

object LiftImplicits {
  implicit val jsCmd: CanRender[JsCmd] = CanRender(JsCmdRenderable)
  implicit val isIE: EventEncoder.CalcIsIE = EventEncoder.CalcIsIE(() => S.request.dmap(false)(_.isIE))
  implicit val domMutationRenderer: LiftDomMutationRenderer.type = LiftDomMutationRenderer
  implicit val config: Config = Config.defaults.copy(domMutationRenderer = domMutationRenderer)
}

case class JsCmdRenderable(cmd: JsCmd) extends Renderable {
  def render = cmd.toJsCmd
}
