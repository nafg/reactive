package reactive
package web
package demo

trait PageSnippet extends Observing {
  implicit lazy val page = Page(
    new AppendToRenderPageComponent(_),
    new SimpleAjaxPageComponent(_),
    new SsePageComponent(_)
  )
}
