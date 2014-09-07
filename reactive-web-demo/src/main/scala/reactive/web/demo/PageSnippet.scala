package reactive
package web
package demo

import net.liftweb.http.S
import reactive.web.lift._

trait PageSnippet extends Observing {
  implicit lazy val page = Page(
    new AppendToRenderTransportType(_),
    new SimpleAjaxTransportType(_),
    // IE is not support Server-Sent Events
    if (S.request.map(_.isIE) openOr false)
      new LiftCometTransportType(_)
    else
      new SseTransportType(_)
  )
}
