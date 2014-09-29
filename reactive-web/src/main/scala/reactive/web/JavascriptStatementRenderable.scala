package reactive
package web

case class JavascriptStatementRenderable(statement: javascript.JsStatement) extends Renderable {
  def render = javascript.JsStatement.render(statement)
}
