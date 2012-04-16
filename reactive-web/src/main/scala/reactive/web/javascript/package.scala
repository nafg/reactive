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

  private class StubInvocationHandler[T <: JsStub: ClassManifest](ident: String) extends InvocationHandler {
    def invoke(proxy: AnyRef, method: Method, args0: Array[AnyRef]): AnyRef = {
      val args = args0 match { case null => Array.empty case x => x }
      val clazz: Class[_] = classManifest[T].erasure

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
          val proxy =
            if (args.isEmpty && (
              classOf[Assignable[_]].isAssignableFrom(method.getReturnType()) ||
              clazz.getMethods.exists(_.getName == method.getName+"_$eq")
            )) {
              new ProxyField(ident, method.getName)
            } else {
              new ApplyProxyMethod(ident, method, clazz, args)
            }

          if (!(classOf[JsStub] isAssignableFrom method.getReturnType())) proxy
          else java.lang.reflect.Proxy.newProxyInstance(
            getClass.getClassLoader,
            method.getReturnType().getInterfaces :+ method.getReturnType(),
            new StubInvocationHandler(proxy.render) {
              override def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef = {
                super.invoke(proxy, method, args)
              }
            }
          )
        }
      }
    }
  }

  /**
   * Returns a JsStub proxy for the specified
   * type. Assumes T's type is the also
   * the name of the instance in the browser.
   */
  def $$[T <: JsStub: ClassManifest]: T =
    jsProxy[T](scalaClassName(classManifest[T].erasure))

  /**
   * Returns a JsStub proxy for the specified type,
   * with the specified identifier for the instance.
   */
  def jsProxy[T <: JsStub: ClassManifest](ident: Symbol): T = jsProxy[T](ident.name)
  def jsProxy[T <: JsStub: ClassManifest](ident: String): T = {
    val ih = new StubInvocationHandler[T](ident)
    java.lang.reflect.Proxy.newProxyInstance(
      getClass.getClassLoader,
      classManifest[T].erasure.getInterfaces :+ classManifest[T].erasure,
      ih
    ).asInstanceOf[T]
  }

  /**
   * A proxy to the browser's window object
   */
  val window = jsProxy[Window]('window)
}
