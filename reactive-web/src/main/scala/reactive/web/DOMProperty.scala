package reactive
package web

import net.liftweb.http.js.{JsCmds, JE, JsCmd, JsExp}
	import JsCmds.SetExp
	import JE.{JsRaw, Str}
import scala.xml.{Elem, MetaData, NodeSeq, Null, UnprefixedAttribute}

import scala.ref.WeakReference

/**
 * Represents a property and/or attribute of a DOM element, synchronized in from the client to the server
 * and updateable on the client via the server
 * @tparam T the type of the value represented by the property
*/
trait DOMProperty[T] extends (NodeSeq=>NodeSeq) {
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
  def eventDataKey = "jsprop" + name
  
  private var owners = List[WeakReference[Owner]]()
  private var eventSources = List[DOMEventSource[_]]()
  
  /**
   * The Page whose ajax event the current thread is responding to, if any
   */
  private val ajaxOwner = new scala.util.DynamicVariable[Option[Owner]](None)
  
  /**
   * The javascript expression that evaluates to the value of this property
   */
  def readJS(id: String): JsExp = JsRaw("document.getElementById('" + id + "')." + name)
  /**
   * The javascript statement to mutate this property
   * @param v the value to mutate it to, as a String
   */
  def writeJS(id: String)(v: String): JsCmd = SetExp(readJS(id), Str(v))
  
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
  def addOwner(id: String)(implicit page: Page): Unit = {
    val owner = Owner(page,id)
    owners ::= new WeakReference(owner)
    
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
        ajaxOwner.withValue(Some(owner)) {
          value.update(fromString(v))
        }
      }
    }
    //apply linked DOM event sources
    //TODO only pages that also own es
    for(owner <- owners; Owner(page,id) <- owner.get; es <- eventSources)
      es.rawEventData += (eventDataKey -> readJS(id))
    // Register setFromAjax with all linked event streams,
    // for the lifetime of the page
    eventSources.foreach(_.rawEventStream.foreach(setFromAjax)(page))
    
    // Whenever the property is updated from a page besides the one
    // being added now, send to all other pages javascript to apply
    // the new value.
    value foreach {v =>
      if(ajaxOwner.value != Some(owner)) {
        for(owner <- owners; Owner(page,id) <- owner.get)  Reactions.inAnyScope(page) {
          Reactions.queue(writeJS(id)(asString(v)))
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
//    println("updateOn: owners=" + owners)
//    println("updateOn: es.rawEventData=" + es.rawEventData)
    eventSources ::= es
  }
  
  /**
   * Returns an Elem with this property applied by adding
   * @param elem
   * @return
   */
  def apply(elem: Elem)(implicit p: Page): Elem = {
    val ret = RElem.withId(elem) % asAttribute
    addOwner(elem attributes("id") text)
    ret
  }
  def apply(in: NodeSeq): NodeSeq = apply(nodeSeqToElem(in))
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
