package reactive.web

trait CanRenderDomMutationConfig extends ConfigBase {
  def domMutationRenderer: CanRender[DomMutation]
}
