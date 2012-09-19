package reactive
package web

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
  def jsProxy[T <: JsStub: ClassManifest](ident: String, toReplace: List[JsStatement] = Nil): T = {
    val ih = new StubInvocationHandler[T](ident, toReplace)
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
