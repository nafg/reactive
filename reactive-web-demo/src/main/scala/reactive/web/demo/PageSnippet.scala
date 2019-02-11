package reactive
package web
package demo

import reactive.web.lift._

trait PageSnippet extends Observing {
  implicit lazy val page: Page = Page(
    new AppendToRenderTransportType(_),
    new SimpleAjaxTransportType(_),
    new SseTransportType(_)
  )
}
