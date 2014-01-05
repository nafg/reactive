package reactive
package web
package demo

trait PageSnippet extends Observing {
  implicit lazy val page = Page(
    _ => new AppendToRenderPageComponent,
    new SimpleAjaxPageComponent(_),
    new SsePageComponent(_)
  )
}
