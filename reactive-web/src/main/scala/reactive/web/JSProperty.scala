package reactive
package web

import net.liftweb.http.js.{JsCmds, JE, JsCmd, JsExp}
	import JsCmds.SetExp
	import JE.{JsRaw, Str}
import scala.xml.{MetaData, Null, UnprefixedAttribute}


/**
 * Represents a javascript property synchronized in from the client to the server
*/
trait JSProperty[T] {
  val value: Var[T]
  
  def name: String
  def elemId: String
  def eventDataKey = "jsprop" + name
  private var pages = List[scala.ref.WeakReference[Page]]()
  private var eventSources = List[JSEventSource[_]]()
  
  /**
   * The Page whose ajax event the current thread is responding to
   */
  private val ajaxPage = new scala.util.DynamicVariable[Option[Page]](None)
  
  def readJS: JsExp = JsRaw("document.getElementById('" + elemId + "')." + name)
  def writeJS(v: String): JsCmd = SetExp(readJS, Str(v))
  
  protected def fromString(s: String): T
  protected def asString(v: T): String
  
  def asAttribute: MetaData = new UnprefixedAttribute(name, asString(value.now), Null)
  
  /**
   * Registers a Page with this JSProperty, so that when
   * @param page
   */
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
    //println("In JSProperty.addPage")
    // Register setFromAjax with all linked event streams,
    // for the lifetime of the page
    eventSources.foreach(_.extraEventStream.foreach(setFromAjax)(page))
    
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
  def updateOn(es: JSEventSource[_]) {
    es.extraEventData += (eventDataKey -> readJS)
    eventSources ::= es
  }
}

trait JSStringProperty extends JSProperty[String] {
  def fromString(s: String) = s
  def asString(v: String) = v
}
