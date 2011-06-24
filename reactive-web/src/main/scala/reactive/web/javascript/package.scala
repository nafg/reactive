package reactive
package web

package object javascript {

  implicit def toJsLiterable[T](x: T)(implicit c: ToJs[T, _]): JsLiterable[T] =
    new JsLiterable[T](x)

}