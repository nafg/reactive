package reactive
package web

import scala.reflect.{ classTag, ClassTag }

import scala.language.implicitConversions

package object javascript {
  import JsTypes._

  implicit def toJsLiterable[T](x: T)(implicit c: ToJsLit[T, _]): JsLiterable[T] =
    new JsLiterable[T](x)
  implicit def toJsIdentable(s: Symbol): JsIdentable = JsIdentable(s)
  implicit def toMatchable[T <: JsAny](against: $[T]) = new Matchable(against)

  implicit def jsExpMethods[T <: JsAny](exp: JsExp[T]): JsExpMethods[T] = new JsExpMethods(exp)

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

  class ProxyName[T : ClassTag](val value: String)
  object ProxyName {
    implicit def fromString[T : ClassTag](s: String): ProxyName[T] = new ProxyName(s)
    implicit def fromSymbol[T : ClassTag](s: Symbol): ProxyName[T] = new ProxyName(s.name)
    implicit def fromUnit[T : ClassTag](s: Unit): ProxyName[T] = new ProxyName(scalaClassName(classTag[T].runtimeClass))
  }

  /**
   * Returns a JsStub proxy for the specified type,
   * with the specified identifier for the instance.
   * @usecase def jsProxy[MyStub](default: Unit): MyStub = ???
   * @usecase def jsProxy[MyStub](symbol: Symbol): MyStub = ???
   * @usecase def jsProxy[MyStub](string: String): MyStub = ???
   */
  def jsProxy[T <: JsStub: ClassTag](ident: ProxyName[T], toReplace: List[JsStatement] = Nil): T = {
    val ih = new StubInvocationHandler[T](ident.value, toReplace)
    java.lang.reflect.Proxy.newProxyInstance(
      getClass.getClassLoader,
      classTag[T].runtimeClass.getInterfaces :+ classTag[T].runtimeClass,
      ih
    ).asInstanceOf[T]
  }

  /**
   * A proxy to the browser's window object
   */
  val window = jsProxy[Window]('window)
}
