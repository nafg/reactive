package reactive
package web

case class Config(domMutationRenderer: CanRender[DomMutation] = DomMutation.defaultDomMutationRenderer)

object Config {
  implicit val defaults = Config()
}
