package reactive
package web
package lift

import net.liftweb.http.S

object LiftImplicits {
  implicit val jsCmd: CanRender[net.liftweb.http.js.JsCmd] = CanRender(_.toJsCmd)
  implicit val isIE = EventEncoder.CalcIsIE(() => S.request.dmap(false)(_.isIE))
  implicit val domMutationRenderer = LiftDomMutationRenderer
  implicit val config = Config.defaults.copy(domMutationRenderer = domMutationRenderer)
}
