package reactive
package web

import scala.xml.{ Elem, MetaData, NodeSeq, Null, UnprefixedAttribute }
import javascript._

import scala.ref.WeakReference

/**
 * Represents a property and/or attribute of a DOM element, synchronized in from the client to the server
 * and updateable on the client via the server.
 * DomProperty is not typed. It has an EventStream[String], 'values', that fires received changes,
 * and an update method that sends updates to the browser as a JsExp.
 * @param name The javascript name of the property
 */
class DomProperty(val name: String)(implicit config: CanRenderDomMutationConfig) extends PageIds {
  class PropertyRenderer(attributeValue: String => Option[String] = _ => None)(implicit page: Page)
    extends ElemFuncWrapper(elem => includedEvents.foldLeft(
      addPage(elem) %
        attributeValue(attributeName).map(
          new UnprefixedAttribute(attributeName, _, Null)
        ).getOrElse(Null)
    ) { (e, es) =>
        es.render(page)(e)
      }
    )

  /**
   * The name when this property is rendered as an attribute.
   * Defaults to name
   */
  def attributeName: String = name

  private val valuesES = new EventSource[String] {}
  def values: EventStream[String] = valuesES

  private var eventSources = List[DomEventSource[_]]()
  private var includedEvents = List[DomEventSource[_ <: DomEvent]]()

  /**
   * This is only used to ensure the event stream is only created once per page
   */
  private val jsEventStreams = new scala.collection.mutable.WeakHashMap[Page, JsEventStream[_]]

  /**
   * The Page and value whose ajax event for this property the current thread is responding to, if any
   */
  private[web] val currentPropagateSource = new scala.util.DynamicVariable[Option[(Page, String)]](None)

  /**
   * The javascript expression that evaluates to the value of this property
   */
  def readJS(id: String): $[JsTypes.JsAny] = JsRaw("document.getElementById(\""+id+"\")."+name)

  /**
   * Registers a Page with this JSProperty.
   * This means that any events linked to this property will
   * have a listener added that will update this property and
   * that is associated with the Page.
   * For every page registered a listener is added to the
   * property value's change EventStream, that will propagate
   * the changes to all other pages.
   * The Page is weakly referenced from this DomProperty.
   * @param page the Page to add. If it exists no action is taken.
   */
  //TODO should events be associated with a Page more directly/explicitly?
  override def addPage(elem: Elem)(implicit page: Page): Elem = {
    val ret = super.addPage(elem)(page)

    /**
     * Causes the value of this property to be updated by extracting its new value
     * from raw event data.
     * Called when any of the linked event streams fires an event, with the
     * raw data of the event, which should contain the new value for this property.
     * With the thread-local 'ajaxPage' set to the Page addPage was called with,
     * the value Var is updated.
     */
    def propagate(v: String) {
      currentPropagateSource.withValue(Some((page, v))) {
        valuesES fire v
        update(v)
      }
    }

    if (eventSources.nonEmpty) jsEventStreams.getOrElseUpdate(page, {
      val jses = new JsEventStream[JsTypes.JsAny]
      for (es <- eventSources)
        es.addEventData(readJS(id), jses)(page)

      // Register propagate with all linked event streams,
      // for the lifetime of the page
      jses.toServer(_.values.toString).foreach(propagate)(page)
      jses
    })

    ret
  }

  /**
   * Link events with this property. The value
   * will be updated on the server whenever an
   * event fires.
   * Events can belong to any Elem, not only the one
   * that this property applies to.
   * @return This DomProperty
   */
  def updateOn(es: DomEventSource[_]*): this.type = {
    eventSources :::= es.toList
    this
  }

  /**
   * Links events with this property. The value
   * will be updated on the server whenever an
   * event fires.
   * Additionally, applying this DomProperty to
   * an Elem will apply the specified DomEventSources
   * too. Therefore events must belong to the same
   * Elem as this property!
   * @return This DomProperty
   */
  def withEvents(es: DomEventSource[_ <: DomEvent]*): this.type = {
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
  def render(implicit page: Page) = new PropertyRenderer()(page)

  /**
   * Attaches this property to an Elem, by adding
   * its corresponding attribute, as well as that of any
   * linked events, and recording its id (adding one if necessary).
   * @return the updated Elem.
   */
  def render(e: Elem)(implicit page: Page): Elem =
    new PropertyRenderer()(page) apply e

  /**
   * Change the value of this property in the browser DOM on all pages
   */
  def update[T: PropertyCodec](value: T) {
    // Send to all other pages javascript to apply
    // the new value, other than the page on which this property started this ajax call, if it's the same value that was sent to us.
    // That is because the browser already has that value.
    def shouldUpdate(page: Page) = currentPropagateSource.value match {
      case Some((`page`, `value`)) => false
      case _                       => true
    }
    for {
      (page, id) <- pageIds
      if shouldUpdate(page)
    } Reactions.inAnyScope(page) {
      Reactions queue DomMutation.UpdateProperty(id, name, attributeName, value)
    }
  }

  override def toString = "DomProperty(name=%s,attributeName=%s)" format (name, attributeName)
}

object DomProperty {
  /**
   * An implicit conversion from DomProperty to NodeSeq=>NodeSeq. Requires an implicit Page. Calls render.
   */
  implicit def toNodeSeqFunc(dp: DomProperty)(implicit page: Page): NodeSeq => NodeSeq = dp.render(page)

  /**
   * An implicit CanForward instance for DomProperty's (does not need to be imported). Requires an implicit Page and PropertyCodec.
   * Values are forwarded by calling DomProperty#update with the return value of codec.toJS applied to the value.
   */
  implicit def canForward[T](implicit codec: PropertyCodec[T]): CanForward[DomProperty, T] = new CanForward[DomProperty, T] {
    def forward(f: Forwardable[T], d: => DomProperty)(implicit o: Observing) = {
      f foreach { v => d.update(v) }
    }
  }
  /**
   * DomProperty factory. Just calls the constructor.
   */
  def apply(name: String)(implicit config: CanRenderDomMutationConfig) = new DomProperty(name)(config)
  /**
   * DomProperty factory. Calls the constructor and overrides attributeName.
   */
  def apply(name: String, attributeName: String)(implicit config: CanRenderDomMutationConfig) = {
    def tmp = attributeName
    new DomProperty(name)(config) {
      override def attributeName = tmp
    }
  }
}
