package reactive
package web

import net.liftweb.http.js.{ JsCmds, JE, JsCmd, JsExp }
import JsCmds.{ SetExp, JsTry }
import JE.JsRaw
import scala.xml.{ Elem, MetaData, NodeSeq, Null, UnprefixedAttribute }

import scala.ref.WeakReference

/**
 * Represents a property and/or attribute of a DOM element, synchronized in from the client to the server
 * and updateable on the client via the server.
 * DOMProperty is not typed. It has an EventStream[String], 'values', that fires received changes,
 * and an update method that sends updates to the browser as a JsExp.
 * @param name The javascript name of the property
 */
class DOMProperty(val name: String) {
  class PropertyRenderer(page: Page, attributeValue: String=>Option[String] = _ => None) extends (NodeSeq=>NodeSeq) {
    def apply(in: NodeSeq): NodeSeq = apply(nodeSeqToElem(in))
    def apply(elem: Elem): Elem = {
      val id = owners.get(page) getOrElse {
        val ret = elem.attributes.get("id").map(_.text) getOrElse Page.newId
        addOwner(ret)
        ret
      }
      includedEvents.foldLeft(
        elem % new UnprefixedAttribute("id", id,
          attributeValue(attributeName).map(
            new UnprefixedAttribute(name, _, Null)
          ).getOrElse(Null)
        )
      ){
        case (e,es) => es(e)
      }
    }
  }
  
  /**
   * The name when this property is rendered as an attribute.
   * Defaults to name
   */
  def attributeName: String = name

  private val valuesES = new EventSource[String] {}
  def values: EventStream[String] = valuesES

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
  private[web] val ajaxPage = new scala.util.DynamicVariable[Option[Page]](None)

  /**
   * The javascript expression that evaluates to the value of this property
   */
  def readJS(id: String): JsExp = JsRaw("document.getElementById('"+id+"')."+name)
  /**
   * The javascript statement to mutate this property
   * @param v the value to mutate it to, as a String
   */
  def writeJS(id: String)(v: JsExp): JsCmd = JsTry(SetExp(readJS(id), v), false)

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
          valuesES fire v
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

  }

  /**
   * Link events with this property. The value
   * will be updated on the server whenever an
   * event fires.
   * Events can belong to any Elem, not only the one
   * that this property applies to.
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
   * Returns a NodeSeq=>NodeSeq that will attach this property
   * to an Elem, by adding
   * any linked events, and recording its id (adding one if necessary),
   * and return the updated Elem.
   */
  def render(implicit page: Page) = new PropertyRenderer(page)

  /**
   * Attaches this property to an Elem, by adding
   * its corresponding attribute, as well as that of any
   * linked events, and recording its id (adding one if necessary).
   * @return the updated Elem.
   */
  def render(e: Elem)(implicit page: Page): Elem =
      new PropertyRenderer(page) apply e
  
  /**
   * Change the value of this property in the browser DOM
   */
  def update(value: JsExp)(implicit page: Page) {
    // Whenever the property is updated from a page besides the one
    // being added now, send to all other pages javascript to apply
    // the new value.
    if (ajaxPage.value != Some(page)) {
      for ((page, id) <- owners) Reactions.inAnyScope(page) {
        Reactions.queue(writeJS(id)(value))
      }
    }
  }
}

object DOMProperty {
  /**
   * An implicit conversion from DOMProperty to NodeSeq=>NodeSeq. Requires an implicit Page. Calls render.
   */
  implicit def toNodeSeqFunc(dp: DOMProperty)(implicit page: Page): NodeSeq => NodeSeq = dp.render(page)

  /**
   * An implicit CanForward instance for DOMProperty's (does not need to be imported). Requires an implicit Page and PropertyCodec.
   * Values are forwarded by calling DOMProperty#update with the return value of codec.toJS applied to the value.
   */
  implicit def canForward[T](implicit page: Page, codec: PropertyCodec[T]): CanForward[DOMProperty, T] = new CanForward[DOMProperty, T] {
    def forward(f: Forwardable[T], d: => DOMProperty)(implicit o: Observing) = {
      f foreach { v => d.update(codec.toJS(v)) }
    }
  }
  /**
   * DOMProperty factory. Just calls the constructor.
   */
  def apply(name: String) = new DOMProperty(name)
  /**
   * DOMProperty factory. Calls the constructor and overrides attributeName.
   */
  def apply(name: String, attributeName: String) = {
    def tmp = attributeName
    new DOMProperty(name) {
      override def attributeName = tmp
    }
  }
}
