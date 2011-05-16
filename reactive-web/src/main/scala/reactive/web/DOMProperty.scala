package reactive
package web

import net.liftweb.http.js.{ JsCmds, JE, JsCmd, JsExp }
import JsCmds.{ SetExp, JsTry }
import JE.{ JsRaw, Str, Num }
import scala.xml.{ Elem, MetaData, NodeSeq, Null, UnprefixedAttribute }

import scala.ref.WeakReference

/**
 * Represents a property and/or attribute of a DOM element, synchronized in from the client to the server
 * and updateable on the client via the server
 * @tparam T the type of the value represented by the property
 */
trait DOMProperty[T] extends (NodeSeq => NodeSeq) {
  private case class Owner(page: Page, id: String)

  /**
   * The Var that represents the property's value
   */
  val value: Var[T]

  /**
   * The javascript name of the property
   */
  def name: String
  /**
   * The value of the property is sent to the server with
   * events using this key in the set of key-value pairs
   */
  def eventDataKey = "jsprop"+name

  private var owners = new scala.collection.mutable.WeakHashMap[Page, String]()
  private var eventSources = List[DOMEventSource[_]]()
  private var includedEvents = List[DOMEventSource[_ <: DOMEvent]]()

  /**
   * The Page whose ajax event the current thread is responding to, if any
   */
  private val ajaxPage = new scala.util.DynamicVariable[Option[Page]](None)

  /**
   * The javascript expression that evaluates to the value of this property
   */
  def readJS(id: String): JsExp = JsRaw("document.getElementById('"+id+"')."+name)
  /**
   * The javascript statement to mutate this property
   * @param v the value to mutate it to, as a String
   */
  def writeJS(id: String)(v: JsExp): JsCmd = JsTry(SetExp(readJS(id), v), false)

  def codec: PropertyCodec[T]

  /**
   * Returns an attribute representing the value of this property, if applicable
   */
  def asAttribute: MetaData = codec.toAttributeValue(name)(value.now) match {
    case Some(v) => new UnprefixedAttribute(name, v, Null)
    case None => Null
  }

  /**
   * Registers a Page with this JSProperty.
   * This means that any events linked to this property will
   * have a listener added that will update this property and
   * that is associated with the Page.
   * For every page registered a listener is added to the
   * property value's change EventStream, that will propagate
   * the changes to all other pages.
   * The Page is wrapped in a WeakReference.
   * @param page the Page to add. If it exists no action is taken.
   */
  //TODO should events be associated with a Page more directly/explicitly?
  def addOwner(id: String)(implicit page: Page): Unit = {
    owners(page) = id

    /**
     * Causes the value of this property to be updated by extracting its new value
     * from raw event data.
     * Called when any of the linked event streams fires an event, with the
     * raw data of the event, which should contain the new value for this property.
     * With the thread-local 'ajaxPage' set to the Page addPage was called with,
     * the value Var is updated.
     */
    def setFromAjax(evt: Map[String, String]) {
      evt.get(eventDataKey) foreach { v =>
        ajaxPage.withValue(Some(page)) {
          value update codec.fromString(v)
        }
      }
    }
    //apply linked DOM event sources
    //TODO only pages that also own es
    for ((page, id) <- owners; es <- eventSources) {
      es.rawEventData += (eventDataKey -> readJS(id))
    }
    // Register setFromAjax with all linked event streams,
    // for the lifetime of the page
    eventSources.foreach(_.rawEventStream.foreach(setFromAjax)(page))

    // Whenever the property is updated from a page besides the one
    // being added now, send to all other pages javascript to apply
    // the new value.
    value foreach { v =>
      if (ajaxPage.value != Some(page)) {
        for ((page, id) <- owners) Reactions.inAnyScope(page) {
          Reactions.queue(writeJS(id)(codec.toJS(v)))
        }
      }
    }
  }

  /**
   * Link events with this property. The value
   * will be updated on the server whenever an
   * event fires.
   * Events can belong to any Elem.
   * @return This DOMProperty
   */
  def updateOn(es: DOMEventSource[_]*): this.type = {
    eventSources :::= es.toList
    this
  }

  /**
   * Links events with this property. The value
   * will be updated on the server whenever an
   * event fires.
   * Additionally, applying this DOMProperty to
   * an Elem will apply the specified DOMEventSources
   * too. Therefore events must belong to the same
   * Elem as this property!
   * @return This DOMProperty
   */
  def withEvents(es: DOMEventSource[_ <: DOMEvent]*): this.type = {
    updateOn(es: _*)
    includedEvents :::= es.toList
    this
  }

  /**
   * Returns an Elem with this property applied by adding
   * its corresponding attribute, as well as that of any
   * linked events
   * @param elem
   * @return
   */
  def apply(elem: Elem)(implicit p: Page): Elem = {
    val e = owners.get(p) match {
      case Some(id) => elem % new UnprefixedAttribute("id", id, asAttribute)
      case None =>
        val withId = RElem.withId(elem) % asAttribute
        addOwner(withId attributes "id" text)
        withId
    }
    includedEvents.foldLeft(e) {
      case (e, es) => es(e)
    }
  }
  def apply(in: NodeSeq): NodeSeq = apply(nodeSeqToElem(in))
}

object DOMProperty {
  class PropertyFactory(name: String) { factory =>
    def apply[T](v: Var[T])(implicit codec0: PropertyCodec[T]) = new DOMProperty[T] {
      val codec = codec0
      val name = factory.name
      val value = v
    }
    def apply[T](initial: T)(onChange: T => Unit)(implicit codec0: PropertyCodec[T], observing: Observing) = new DOMProperty[T] {
      val codec = codec0
      val name = factory.name
      val value = Var(initial)
      value.change foreach onChange
    }
  }
  def apply(name: String) = new PropertyFactory(name)
}

/**
 * Instances of this trait specify how to transport element property values to and from the client.
 */
trait PropertyCodec[T] {
  /**
   * Get a T from the String representation sent via ajax with events (via DOMEventSource.rawEventData)
   */
  def fromString: String => T
  /**
   * How to send the value as JavaScript to the browser via ajax or comet
   */
  def toJS: T => JsExp
  /**
   * The attribute value to initialize the property's value, or None for no attribute
   */
  def toAttributeValue(propName: String): T => Option[String]
}

object PropertyCodec {
  implicit val string: PropertyCodec[String] = new PropertyCodec[String] {
    def fromString = s => s
    val toJS = Str
    def toAttributeValue(propName: String) = Some(_)
  }
  implicit val int: PropertyCodec[Int] = new PropertyCodec[Int] {
    def fromString = _.toInt
    val toJS = Num(_: Int)
    def toAttributeValue(propName: String) = (v: Int) => Some(v.toString)
  }
  implicit val intOption: PropertyCodec[Option[Int]] = new PropertyCodec[Option[Int]] {
    def fromString = _.toInt match { case -1 => None case n => Some(n) }
    val toJS = (io: Option[Int]) => Num(io getOrElse -1)
    def toAttributeValue(propName: String) = _.map(_.toString)
  }
  implicit val boolean: PropertyCodec[Boolean] = new PropertyCodec[Boolean] {
    def fromString = _.toLowerCase match {
      case "" | "false" | net.liftweb.util.Helpers.AsInt(0) => false
      case _ => true
    }
    def toJS = (b: Boolean) => if (b) JE.JsTrue else JE.JsFalse
    def toAttributeValue(propName: String) = (v: Boolean) => if (v) Some(propName) else None
  }
}

