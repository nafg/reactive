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

  private[javascript] def classToIdent(c: Class[_]) = c.getName.toList.reverse.dropWhile('$'==).takeWhile('$'!=).reverse.mkString

  def $$[T <: JsStub : Manifest]: T = {
    val ih = new java.lang.reflect.InvocationHandler {
      def invoke(proxy: AnyRef, method: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef = {
        JsRaw(classToIdent(manifest[T].erasure)+"."+method.getName +
          (if (args.isEmpty) ""
          else args.map(_.asInstanceOf[JsExp[_]].render).mkString("(", ",", ")"))
        )
      }
    }
    java.lang.reflect.Proxy.newProxyInstance(
      getClass.getClassLoader,
      manifest.erasure.getInterfaces :+ manifest.erasure,
      ih
    ).asInstanceOf[T]
  }
}
