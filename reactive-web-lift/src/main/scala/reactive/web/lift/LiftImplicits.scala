package reactive
package web
package lift

import net.liftweb.http.S
import net.liftweb.http.js.JsCmd

object LiftImplicits {
  implicit val jsCmd: CanRender[JsCmd] = CanRender(JsCmdRenderable(_))
  implicit val isIE = EventEncoder.CalcIsIE(() => S.request.dmap(false)(_.isIE))
  implicit val domMutationRenderer = LiftDomMutationRenderer
  implicit val config = Config.defaults.copy(domMutationRenderer = domMutationRenderer)
}

case class JsCmdRenderable(cmd: JsCmd) extends Renderable {
  def render = cmd.toJsCmd
}
