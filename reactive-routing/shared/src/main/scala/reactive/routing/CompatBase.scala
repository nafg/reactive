package reactive.routing

private[routing] trait CompatBase {
  def encodeURIComponent: String => String
}
