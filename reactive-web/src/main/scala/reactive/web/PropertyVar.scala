package reactive
package web

import scala.xml.{ Elem, MetaData, NodeSeq, Null, UnprefixedAttribute }

import javascript._

/**
 * Instances of this trait specify how to encode element property values to the client
 * and decode them from the client.
 * There should be one instance for each DOM attribute data type.
 * PropertyCodec is not for general purpose type conversions. For example,
 * if you want to edit numerical ranges such as "2,5-7" in a text field,
 * you would not have a PropertyVar and PropertyCodec of some custom type.
 * Rather, you would use a PropertyVar[String] and the provided PropertyCodec[String],
 * and then externally synchronize the PropertyVar with your custom representation.
 * @tparam T the Scala type representation
 */
trait PropertyCodec[T] {
  /**
   * Get a T from the String representation sent via ajax with events (via PropertyVar.withEvents/updateOn)
   */
  def fromString: String => T
  /**
   * How to send the value as JavaScript to the browser
   */
  def toJS: T => $[JsTypes.JsAny]
  /**
   * The attribute value to initialize the property with (None for no attribute)
   * value => name => attribute
   */
  def toAttributeValue: T => String => Option[String]
}

object PropertyCodec {
  implicit val string: PropertyCodec[String] = new PropertyCodec[String] {
    def fromString = s => s
    val toJS = implicitly[String => $[JsTypes.JsString]]
    def toAttributeValue = v => _ => Some(v)
  }
  implicit val int: PropertyCodec[Int] = new PropertyCodec[Int] {
    def fromString = _.toInt
    val toJS = implicitly[Int => $[JsTypes.JsNumber]]
    def toAttributeValue = (v: Int) => _ => Some(v.toString)
  }
  implicit val intOption: PropertyCodec[Option[Int]] = new PropertyCodec[Option[Int]] {
    def fromString = _.toInt match { case -1 => None case n => Some(n) }
    val toJS = { io: Option[Int] =>
      val i = implicitly[Int => $[JsTypes.JsNumber]]
      i(io getOrElse -1: Int)
    }
    def toAttributeValue = v => _ => v.map(_.toString)
  }
  implicit val boolean: PropertyCodec[Boolean] = new PropertyCodec[Boolean] {
    def fromString = _.toLowerCase match {
      case "" | "false" | net.liftweb.util.Helpers.AsInt(0) => false
      case _ => true
    }
    def toJS = (b: Boolean) => if (b) true.$ else false.$
    def toAttributeValue = (v: Boolean) => name => if (v) Some(name) else None
  }
}

object PropertyVar {
  /**
   * Intermediate step in creating a PropertyVar.
   * The PropertVarFactory has the underlying stateless DomProperty
   * and provides several apply methods to instantiate a PropertyVar that wraps it.
   */
  class PropertyVarFactory(name: String, attributeName: String) {
    private def dom(implicit config: CanRenderDomMutationConfig) = DomProperty(name, attributeName)(config)

    /**
     * A `PropertyVar` synced from a `Signal`
     * @tparam A        The type of the `Signal` and the returned `PropertyVar`
     * @param  signal   A signal whose value the `PropertyVar` should reflect
     * @return          A `PropertyVar[A]` with the specified time-varying value
     * @example {{{PropertyVar.value fromSignal mySignal}}}
     */
    def fromSignal[A](signal: Signal[A])(implicit codec: PropertyCodec[A], observing: Observing, config: CanRenderDomMutationConfig): PropertyVar[A] = new PropertyVar[A](dom(config))(signal.now) <<: signal.change

    /**
     * A function that renders a `PropertyVar` synced from a `Signal` based on the attribute's initial value
     * @tparam A        The type of the attribute/property for which an (implicit) `PropertyCodec` is available
     * @param  f        A function from the optional current attribute value (at the time of rendering) to the desired time-varying value
     * @return          An `ElemFuncWrapper` (which extends `NodeSeq=>NodeSeq`) that renders a `PropertyVar` according to `f`.
     * @example {{{
     *  // See also PropertyVar.appendClass, which is a convenient shortcut for the following
     *  PropertyVar.className transform { s: Option[String] =>
     *    valid.map(if(_) s.getOrElse("") else s.getOrElse("")+" invalid")  // add 'invalid' class to existing class attribute value
     *  }
     * }}}
     */
    def transform[A](f: Option[String] => Signal[A])(implicit codec: PropertyCodec[A], observing: Observing, config: CanRenderDomMutationConfig, page: Page): ElemFuncWrapper = new ElemFuncWrapper({ elem =>
      val domProp = dom(config)
      val s = f(elem.attributes.asAttrMap get domProp.attributeName)
      val pv = new PropertyVar[A](domProp)(s.now) <<: s
      pv.render(elem)
    })

    /**
     * A PropertyVar initialized with a plain value
     * @tparam T   The type of the PropertyVar and PropertyCodec
     * @tparam A   The type of init
     * @param init The initial value to apply the PropertyVarInit to.
     * @return     A PropertyVar with the specified constant value.
     * @example {{{PropertyVar("size")(80)}}}
     */
    def apply[A](init: A)(implicit codec: PropertyCodec[A], observing: Observing, config: CanRenderDomMutationConfig): PropertyVar[A] = new PropertyVar[A](dom(config))(init)
  }

  /**
   * Returns a PropertVarFactory.
   * @example PropertyVar(name)(init)
   * @param name the attribute and property name of the DomProperty
   */
  def apply(name: String) = new PropertyVarFactory(name, name)
  /**
   * Returns a PropertyVarFactory.
   * @example PropertyVar(name, attrName)(init)
   * @param name the property name of the DomProperty (used for javascript reads and writes)
   * @param attributeName the attribute name of the DomProperty (used for initial rendering of element)
   */
  def apply(name: String, attributeName: String) = new PropertyVarFactory(name, attributeName)

  /**
   * An implicit conversion from PropertyVar to NodeSeq=>NodeSeq. Requires an implicit Page. Calls render.
   */
  implicit class toNodeSeqFunc(dp: PropertyVar[_])(implicit page: Page) extends (NodeSeq => NodeSeq) {
    def apply(ns: NodeSeq) = dp.render(page)(ns)
  }

  /**
   * Convenience shortcut for PropertyVar("value"), i.e., returns a PropertyVarFactory for the value attribute/property
   */
  def value = apply("value")

  /**
   * Convenience shortcut for PropertyVar("className", "class"), i.e., returns a PropertyVarFactory for the className property / class attribute
   */
  def className = apply("className", "class")

  /**
   * Convenience shortcut to append optional text to the class attribute based on a Signal[String].
   * Note that multiple invocations of appendClass for the same element will override each other,
   * so either unify all the classes you may want to add into one signal, if possible,
   * or use [[toggleClass]] if applicable.
   * @see [[toggleClass]].
   * @example {{{  "input" #> appendClass(isValid map {v => Some("invalid") filter (_ => v) }) }}}
   */
  def appendClass(s: Signal[Option[String]])(implicit observing: Observing, page: Page) = className transform { cs: Option[String] =>
    s map { _ map { c => cs.filter(_.nonEmpty) map (_+" "+c) getOrElse c } getOrElse cs.getOrElse(""): String }
  }

  //TODO use js api
  //TODO does it belong in PropertyVar?
  /**
   * A `NodeSeq` function (using [[RElem.withElemId]])
   * that will toggle a class on and off in response to a `Boolean` `Signal`.
   * @param cls the class to toggle
   * @param signal the `Signal` that indicates when to toggle the class on and when to toggle it off
   * @return a function that will toggle the class of the given element
   */
  def toggleClass(cls: String)(signal: Signal[Boolean])(implicit page: Page, observing: Observing): NodeSeq => NodeSeq = RElem.withElemId { id =>
    signal
      .map { b =>
        s"(function(){var e=document.getElementById('$id');" +
          (
            if (b)
              s"if(!/\\b$cls\\b/.test(e.className)) e.className+=' $cls'"
            else
              s"e.className=e.className.replace(/\\b$cls\\b/, '')"
          ) +
            ";})()"
      }
      .foreach(page.queue)
    identity
  }
}

/**
 * Wraps a DomProperty as a type-safe Var.
 * @param dom the DomProperty to wrap
 * @param init the initial value (rendered in the attribute)
 */
class PropertyVar[T](val dom: DomProperty)(init: T)(implicit codec: PropertyCodec[T], observing: Observing) extends Var(init) {
  (this >> dom) <<: dom.values.map(codec.fromString)

  override def debugName = "PropertyVar(%s)(%s)" format (dom, now)

  /**
   * Wraps a new DomProperty as a type-safe Var.
   * @param name the name of the DomProperty to create and wrap
   * @param init the initial value (rendered in the attribute)
   */
  @deprecated("Use the factory: PropertyVar(name)(init)", "0.2")
  def this(name: String)(init: T)(implicit codec: PropertyCodec[T], observing: Observing, config: CanRenderDomMutationConfig) = this(DomProperty(name)(config))(init)(codec, observing)
  /**
   * Wraps a new DomProperty as a type-safe Var.
   * @param name the name of the DomProperty to create and wrap
   * @param attributeName the name of the attribute rendered by the DomProperty
   * @param init the initial value (rendered in the attribute)
   */
  @deprecated("Use the factory: PropertyVar(name, attributeName)(init)", "0.2")
  def this(name: String, attributeName: String)(init: T)(implicit codec: PropertyCodec[T], observing: Observing, config: CanRenderDomMutationConfig) =
    this(DomProperty(name, attributeName)(config))(init)(codec, observing)

  /**
   * Returns a NodeSeq=>NodeSeq that will attach this property
   * to an Elem, by adding
   * its corresponding attribute, as well as that of any
   * linked events, and recording its id (adding one if necessary),
   * and return the updated Elem.
   */
  def render(implicit page: Page) =
    new dom.PropertyRenderer(codec.toAttributeValue(now))(page)

  /**
   * Attaches this property to an Elem, by adding
   * its corresponding attribute, as well as that of any
   * linked events, and recording its id (adding one if necessary).
   * @return the updated Elem.
   */
  def render(e: Elem)(implicit page: Page): Elem =
    new dom.PropertyRenderer(codec.toAttributeValue(now))(page) apply e

  /**
   * Link events with this property. The value
   * will be updated on the server whenever an
   * event fires.
   * Events can belong to any Elem, not only the one
   * that this property applies to.
   * @return This PropertyVar
   */
  def updateOn(es: DomEventSource[_]*) = { dom.updateOn(es: _*); this }

  /**
   * Links events with this property. The value
   * will be updated on the server whenever an
   * event fires.
   * Additionally, applying this DomProperty to
   * an Elem will apply the specified DomEventSources
   * too. Therefore events must belong to the same
   * Elem as this property!
   * @return This PropertyVar
   */
  def withEvents(es: DomEventSource[_ <: DomEvent]*) = { dom.withEvents(es: _*); this }
}
