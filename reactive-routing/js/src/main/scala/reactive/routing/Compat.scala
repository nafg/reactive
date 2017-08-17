package reactive.routing

import scala.scalajs.js.URIUtils


private[routing] object Compat extends CompatBase {
  def encodeURIComponent: String => String = URIUtils.encodeURIComponent
}
