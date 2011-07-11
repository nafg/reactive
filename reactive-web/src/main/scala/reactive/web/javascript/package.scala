package reactive
package web

import java.lang.reflect.{ InvocationHandler, Proxy }

package object javascript {
  import JsTypes._

  implicit def toJsLiterable[T](x: T)(implicit c: ToJsLit[T, _]): JsLiterable[T] =
    new JsLiterable[T](x)
  implicit def toJsIdentable(s: Symbol): JsIdentable = JsIdentable(s)

  type $[+T <: JsAny] = JsExp[T]

  type =|>[-P <: JsAny, +R <: JsAny] = JsTypes.JsFunction1[P, R]

  def $[T <: JsAny] = JsIdent.fresh[T]
  def $[T <: JsAny](name: Symbol) = JsIdent[T](name)

  private[javascript] def classToIdent(c: Class[_]) = {
    val name = c.getSimpleName
    val lastDollar = name.lastIndexOf('$')
    val dropEnd = if (name.substring(lastDollar + 1) forall (_.isDigit)) name.length - lastDollar else 0
    name.toList.reverse.drop(dropEnd).takeWhile('$'!=).reverse.mkString
  }

  private class StubInvocationHandler[T <: JsStub: Manifest](ident: String) extends InvocationHandler {
    def invoke(proxy: AnyRef, method: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef = {
      val clazz: Class[_] = manifest[T].erasure
      def hasField = try {
        clazz.getField(method.getName)
        true
      } catch {
        case _: NoSuchFieldException =>
          false
      }
      try { // look for static forwarder
        val forwarder = Class.
          forName(clazz.getName+"$class").
          getMethod(method.getName, clazz +: method.getParameterTypes: _*)
        forwarder.invoke(null, proxy +: args: _*)
      } catch {
        case _: NoSuchMethodException | _: ClassNotFoundException =>
          new JsRaw(ident+"."+method.getName + {
            if (args.isEmpty && hasField)
              ""
            else if (method.getParameterTypes.forall(classOf[JsExp[_]].isAssignableFrom))
              args.map(_.asInstanceOf[JsExp[_]].render).mkString("(", ",", ")")
            else
              args.map(_.toString).mkString("(", ",", ")")
          }) with JsStatement {
            def toReplace = args.collect{ case x: JsExp[_] => x }.toList
          }
      }
    }
  }
  def $$[T <: JsStub: Manifest]: T =
    JsStub[T](classToIdent(manifest[T].erasure))

  def JsStub[T <: JsStub: Manifest](ident: String): T = {
    val ih = new StubInvocationHandler(ident)
    java.lang.reflect.Proxy.newProxyInstance(
      getClass.getClassLoader,
      manifest.erasure.getInterfaces :+ manifest.erasure,
      ih
    ).asInstanceOf[T]
  }

  val window = JsStub[Window]("window")
}
