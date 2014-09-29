package reactive
package web

case class Config(domMutationRenderer: CanRender[DomMutation] = DomMutation.defaultDomMutationRenderer)
  extends CanRenderDomMutationConfig

object Config {
  implicit val defaults = Config()
}
