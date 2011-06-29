package reactive
package web

package object javascript {
  import JsTypes._

  implicit def toJsLiterable[T](x: T)(implicit c: ToJsLit[T, _]): JsLiterable[T] =
    new JsLiterable[T](x)
  implicit def toJsIdentable(s: Symbol): JsIdentable = JsIdentable(s)

  type $[+T <: JsAny] = JsExp[T]

  type =|>[-P <: JsAny, +R <: JsAny] = JsTypes.JsFunction1[P, R]

  def $[T <: JsAny] = JsIdent.fresh[T]
  def $[T <: JsAny](name: Symbol) = JsIdent[T](name)
}
