package reactive
package web

import net.liftweb.http.js.{JsCmds, JE, JsCmd, JsExp}
	import JsCmds.SetExp
	import JE.{JsRaw, Str}
import scala.xml.{MetaData, Null, UnprefixedAttribute}


/**
 * Represents a property and/or attribute of a DOM element, synchronized in from the client to the server
 * and updateable on the client via the server
 * @tparam T the type of the value represented by the property
*/
trait DOMProperty[T] {
  /**
   * The Var that represents the property's value
   */
  val value: Var[T]
  
  /**
   * The javascript name of the property
   */
  def name: String
  /**
   * The id of the element this property belongs to
   */
  def elemId: String
  /**
   * The value of the property is sent to the server with
   * events using this key in the set of key-value pairs
   */
  def eventDataKey = "jsprop" + name
  
  private var pages = List[scala.ref.WeakReference[Page]]()
  private var eventSources = List[DOMEventSource[_]]()
  
  /**
   * The Page whose ajax event the current thread is responding to
   */
  private val ajaxPage = new scala.util.DynamicVariable[Option[Page]](None)
  
  /**
   * The javascript expression that evaluates to the value of this property
   */
  def readJS: JsExp = JsRaw("document.getElementById('" + elemId + "')." + name)
  /**
   * The javascript statement to mutate this property
   * @param v the value to mutate it to, as a String
   */
  def writeJS(v: String): JsCmd = SetExp(readJS, Str(v))
  
  /**
   * How to get a T from the String sent via ajax with events
   */
  protected def fromString(s: String): T
  /**
   * How to get a String to put in the attribute or send to the browser via ajax or comet, given a T
   */
  protected def asString(v: T): String
  
  /**
   * Returns an attribute representing the value of this property, if applicable
   */
  def asAttribute: MetaData = new UnprefixedAttribute(name, asString(value.now), Null)
  
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
  def addPage(implicit page: Page): Unit = if(!pages.exists(_.get==Some(page))) {
    /**
     * Causes the value of this property to be updated by extracting its new value
     * from raw event data.
     * Called when any of the linked event streams fires an event, with the
     * raw data of the event, which should contain the new value for this property.
     * With the thread-local 'ajaxPage' set to the Page addPage was called with,
     * the value Var is updated.
     */
    def setFromAjax(evt: Map[String,String]) {
      evt.get(eventDataKey) foreach {v =>
        ajaxPage.withValue(Some(page)) {
          value.update(fromString(v))
        }
      }
    }
    // Register setFromAjax with all linked event streams,
    // for the lifetime of the page
    eventSources.foreach(_.rawEventStream.foreach(setFromAjax)(page))
    
    // Whenever the property is updated from a page besides the one
    // being added now, send to all other pages javascript to apply
    // the new value.
    value foreach {v =>
      if(ajaxPage.value != Some(page)) {
        for(page <- pages.flatMap(_.get)) Reactions.inAnyScope(page) {
          Reactions.queue(writeJS(asString(v)))
        }
      }
    }
  }
  
  /**
   * Link an event with this property. The value
   * will be updated on the server whenever an
   * event fires.
   */
  def updateOn(es: DOMEventSource[_]) {
    es.rawEventData += (eventDataKey -> readJS)
    eventSources ::= es
  }
}

/**
 * Provides identity conversions
 */
//TODO use implicit objects
trait DOMStringProperty extends DOMProperty[String] {
  def fromString(s: String) = s
  def asString(v: String) = v
}

/**
 * Provides conversions between Int and String
 */
trait DOMIntProperty extends DOMProperty[Int] {
  def default = 0
  def fromString(s: String) = try{s.toInt} catch {case _ => default}
  def asString(v: Int) = v.toString
}
