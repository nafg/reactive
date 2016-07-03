package reactive.routing

import java.net.URLEncoder


private[routing] object Compat extends CompatBase {
  val encodeURIComponent: String => String = URLEncoder.encode(_: String, "UTF-8")
}
