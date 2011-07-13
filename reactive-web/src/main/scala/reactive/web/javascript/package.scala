package reactive
package web

import java.lang.reflect.{ InvocationHandler, Proxy }

package object javascript {
  import JsTypes._

  implicit def toJsLiterable[T](x: T)(implicit c: ToJsLit[T, _]): JsLiterable[T] =
    new JsLiterable[T](x)
  implicit def toJsIdentable(s: Symbol): JsIdentable = JsIdentable(s)

  /**
   * A type alias for JsExp
   */
  type $[+T <: JsAny] = JsExp[T]

  /**
   * A type alias for JsFunction1
   */
  type =|>[-P <: JsAny, +R <: JsAny] = JsTypes.JsFunction1[P, R]

  /**
   * Returns a JsIdent with a fresh name
   */
  def $[T <: JsAny] = JsIdent.fresh[T]
  /**
   * Returns a JsIdent with the specified name
   */
  def $[T <: JsAny](name: Symbol) = JsIdent[T](name)

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

  /**
   * Returns a JsStub proxy for the specified
   * type. Assumes T's type is the also
   * the name of the instance in the browser.
   */
  def $$[T <: JsStub: Manifest]: T =
    jsProxy[T](scalaClassName(manifest[T].erasure))

  /**
   * Returns a JsStub proxy for the specified type,
   * with the specified identifier for the instance. 
   */
  def jsProxy[T <: JsStub: Manifest](ident: Symbol): T = jsProxy[T](ident.name)
  def jsProxy[T <: JsStub: Manifest](ident: String): T = {
    val ih = new StubInvocationHandler(ident)
    java.lang.reflect.Proxy.newProxyInstance(
      getClass.getClassLoader,
      manifest.erasure.getInterfaces :+ manifest.erasure,
      ih
    ).asInstanceOf[T]
  }

  /**
   * A proxy to the browser's window object
   */
  val window = jsProxy[Window]('window)
}
