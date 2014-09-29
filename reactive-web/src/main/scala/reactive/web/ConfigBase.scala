package reactive
package web

object ConfigBase {
  implicit val defaults: ConfigBase = Config.defaults
}

trait ConfigBase
