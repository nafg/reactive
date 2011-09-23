package reactive
package web

import scala.xml.{ Elem, MetaData, NodeSeq, Null, UnprefixedAttribute }

import javascript._


/**
 * Instances of this trait specify how to transport element property values to and from the client.
 * @tparam T the Scala type representation
 * @tparam C the context --- used to distinguish between several PropertyCodecs of the same type
 */
trait PropertyCodec[T] {
  /**
   * Get a T from the String representation sent via ajax with events (via DOMEventSource.rawEventData)
   */
  def fromString: String => T
  /**
   * How to send the value as JavaScript to the browser via ajax or comet
   */
  def toJS: T => $[JsTypes.JsAny]
  /**
   * The attribute value to initialize the property's value, or None for no attribute
   */
  def toAttributeValue: T => String => Option[String]
}

object PropertyCodec {
  implicit val string: PropertyCodec[String] = new PropertyCodec[String] {
    def fromString = s => s
    val toJS = ToJs.string
    def toAttributeValue = v => _ => Some(v)
  }
  implicit val int: PropertyCodec[Int] = new PropertyCodec[Int] {
    def fromString = _.toInt
    val toJS = ToJs.int(_:Int)
    def toAttributeValue = (v: Int) => _ => Some(v.toString)
  }
  implicit val intOption: PropertyCodec[Option[Int]] = new PropertyCodec[Option[Int]] {
    def fromString = _.toInt match { case -1 => None case n => Some(n) }
    val toJS = (io: Option[Int]) => ToJs.int(io getOrElse -1: Int)
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
   * Wrap a DOMProperty as a type-safe Var.
   * @param dom the DOMProperty to wrap
   * @param init the initial value (rendered in the attribute)
   */
  def apply[T](dom: DOMProperty)(init: T)(implicit codec: PropertyCodec[T], observing: Observing): PropertyVar[T] = new PropertyVar[T](dom)(init)(codec, observing)

  /**
   * Wrap a new DOMProperty as a type-safe Var.
   * @param name the name of the DOMProperty to create and wrap
   * @param init the initial value (rendered in the attribute)
   */
  def apply[T](name: String)(init: T)(implicit codec: PropertyCodec[T], observing: Observing): PropertyVar[T] = new PropertyVar[T](name)(init)(codec, observing)
  /**
   * Wraps a new DOMProperty as a type-safe Var.
   * @param name the name of the DOMProperty to create and wrap
   * @param attributeName the name of the attribute rendered by the DOMProperty
   * @param init the initial value (rendered in the attribute)
   */
  def apply[T](name: String, attributeName: String)(init: T)(implicit codec: PropertyCodec[T], observing: Observing): PropertyVar[T] =
    new PropertyVar[T](name, attributeName)(init)(codec, observing)

  /**
   * An implicit conversion from PropertyVar to NodeSeq=>NodeSeq. Requires an implicit Page. Calls render.
   */
  implicit def toNodeSeqFunc(dp: PropertyVar[_])(implicit page: Page): NodeSeq => NodeSeq = dp.render(page)
}

/**
 * Wraps a DOMProperty as a type-safe Var.
 * @param dom the DOMProperty to wrap
 * @param init the initial value (rendered in the attribute)
 */
class PropertyVar[T](val dom: DOMProperty)(init: T)(implicit codec: PropertyCodec[T], observing: Observing) extends Var(init) {
  (this >> dom) <<: dom.values.map(codec.fromString)

  override def debugName = "PropertyVar(%s)(%s)" format (dom, now)

  /**
   * Wraps a new DOMProperty as a type-safe Var.
   * @param name the name of the DOMProperty to create and wrap
   * @param init the initial value (rendered in the attribute)
   */
  def this(name: String)(init: T)(implicit codec: PropertyCodec[T], observing: Observing) = this(DOMProperty(name))(init)(codec, observing)
  /**
   * Wraps a new DOMProperty as a type-safe Var.
   * @param name the name of the DOMProperty to create and wrap
   * @param attributeName the name of the attribute rendered by the DOMProperty
   * @param init the initial value (rendered in the attribute)
   */
  def this(name: String, attributeName: String)(init: T)(implicit codec: PropertyCodec[T], observing: Observing) =
    this(DOMProperty(name, attributeName))(init)(codec, observing)

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
  def updateOn(es: DOMEventSource[_]*) = { dom.updateOn(es: _*); this }

  /**
   * Links events with this property. The value
   * will be updated on the server whenever an
   * event fires.
   * Additionally, applying this DOMProperty to
   * an Elem will apply the specified DOMEventSources
   * too. Therefore events must belong to the same
   * Elem as this property!
   * @return This PropertyVar
   */
  def withEvents(es: DOMEventSource[_ <: DOMEvent]*) = { dom.withEvents(es: _*); this }
}
