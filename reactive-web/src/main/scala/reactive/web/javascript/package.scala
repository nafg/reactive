package reactive
package web

import java.lang.reflect.{ InvocationHandler, Proxy, Method }

package object javascript {
  import JsTypes._

  implicit def toJsLiterable[T](x: T)(implicit c: ToJsLit[T, _]): JsLiterable[T] =
    new JsLiterable[T](x)
  implicit def toJsIdentable(s: Symbol): JsIdentable = JsIdentable(s)
  implicit def toMatchable[T <: JsAny](against: $[T]) = new Matchable(against)

  /**
   * A type alias for JsExp
   */
  type $[+T <: JsAny] = JsExp[T]

  /**
   * A type alias for JsFunction1
   */
  type =|>[-P <: JsAny, +R <: JsAny] = JsTypes.JsFunction1[P, R]

  implicit def toForInable[T <: JsAny](exp: JsExp[JsArray[T]]) = ForInable(exp)
  def Each[T <: JsAny](exp: $[JsArray[T]]) = ForEachInable(exp)
  def Break = new Break

  /**
   * Returns a JsIdent with the specified name
   */
  def $[T <: JsAny](name: Symbol) = JsIdent[T](name)

  private class StubInvocationHandler[T <: JsStub: Manifest](ident: String) extends InvocationHandler {
    def invoke(proxy: AnyRef, method: Method, args0: Array[AnyRef]): AnyRef = {
      val args = args0 match { case null => Array.empty case x => x }
      val clazz: Class[_] = manifest[T].erasure

      // look for static forwarder --- that means the method has a scala method body, so invoke it
      def findAndInvokeForwarder(clazz: Class[_]): Option[Method] = try {
        Some(
          Class.
            forName(clazz.getName+"$class").
            getMethod(method.getName, clazz +: method.getParameterTypes: _*)
        )
      } catch {
        case _: NoSuchMethodException | _: ClassNotFoundException =>
          clazz.getInterfaces().map(findAndInvokeForwarder).find(_.isDefined) getOrElse (
            clazz.getSuperclass match {
              case null => None
              case c    => findAndInvokeForwarder(c)
            }
          )
      }
      //TODO hack
      if (method.getName == "render" && method.getReturnType == classOf[String] && args.isEmpty)
        ident
      else {
        findAndInvokeForwarder(clazz).map(_.invoke(null, proxy +: args: _*)) getOrElse {
          if (classOf[JsStub] isAssignableFrom method.getReturnType()) {
            val id = ident+"."+method.getName()
            val ih = new StubInvocationHandler(id) {
              override def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef = {
                super.invoke(proxy, method, args)
              }
            }
            java.lang.reflect.Proxy.newProxyInstance(
              getClass.getClassLoader,
              method.getReturnType().getInterfaces :+ method.getReturnType(),
              ih
            )
          } else {
            def hasField = try {
              clazz.getField(method.getName); true
            } catch {
              case _: NoSuchFieldException => false
            }
            if (args.isEmpty && classOf[Assignable[_]].isAssignableFrom(method.getReturnType()))
              new ProxyField(ident, method.getName)
            else
              new ApplyProxyMethod(ident, method, clazz, args)
          }
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
