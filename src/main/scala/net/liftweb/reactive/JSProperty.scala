package net.liftweb.reactive

import _root_.reactive._
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
  
  private val ajaxPage = new scala.util.DynamicVariable[Option[Page]](None)
  
  def readJS: JsExp = JsRaw("document.getElementById('" + elemId + "')." + name)
  def writeJS(v: String): JsCmd = SetExp(readJS, Str(v))
  
  protected def fromString(s: String): T
  protected def asString(v: T): String
  
  def asAttribute: MetaData = new UnprefixedAttribute(name, asString(value.now), Null)
  
  def addPage(implicit page: Page): Unit = if(!pages.exists(_.get==Some(page))) {
    def setFromAjax(evt: Map[String,String]) {
      evt.get(eventDataKey) foreach {v =>
        ajaxPage.withValue(Some(page)){
          value.update(fromString(v))
        }
      }
    }
    //println("In JSProperty.addPage")
    eventSources.foreach(_.extraEventStream.foreach(setFromAjax)(page))
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
