package reactive
package web

trait ConfigBase
object ConfigBase {
  implicit val defaults: Config = Config.defaults
}
trait CanRenderDomMutationConfig extends ConfigBase {
  def domMutationRenderer: CanRender[DomMutation]
}

case class Config(domMutationRenderer: CanRender[DomMutation] = DomMutationRenderer.default)
  extends CanRenderDomMutationConfig


object Config {
  implicit val defaults: Config = Config()
}
