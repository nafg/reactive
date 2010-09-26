package net.liftweb.reactive


import net.liftweb.http._
  import js._
    import JsCmds._
    import JE._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.actor._
import scala.xml._

import _root_.reactive._



object RElem {
  private case class ElemWrapper(parent: Elem, val children: RElem*) extends RElem {
    val (baseElem, _id) = parent.attributes.get("id") match {
      case Some(id) =>
        (parent, id.text)
      case None =>
        val id = randomString(20)
        (parent % new UnprefixedAttribute("id", id, Null), id)
    }
    override lazy val id = _id
    val properties, events = Nil
    override def render = baseElem.copy(child = baseElem.child ++ children.map(_.render))
  }
  private[reactive] val elems = new scala.collection.mutable.WeakHashMap[String,RElem]
  
  def ajaxFunc(f: String=>Unit): List[String]=>JsCmd = {
    case Nil => JsCmds.Noop
    case s :: _ => Reactions.inClientScope(f(s))
  }
  
  def apply(parent: Elem, children: RElem*): RElem = new ElemWrapper(parent, children: _*)
  def apply(text: String): RElem = new ElemWrapper(<span>{text}</span>)
  implicit def relemsToNS(relems: Seq[RElem]): NodeSeq = relems.map(_.render)
  
  implicit def unboxNS: Box[NodeSeq] => NodeSeq = _.openOr(NodeSeq.Empty)
  class Matcher(pre: String) {
    implicit def bindParamsToPF(seq: Seq[BindParam]): PartialFunction[Node,NodeSeq] = {
      import _root_.scala.collection.immutable.{Map, HashMap}
      val map: Map[String, BindParam] = HashMap.empty ++ seq.map(p => (p.name, p));
      
      { case e @ Elem(`pre`, label, _, _, child @ _*) if map.contains(label) =>
           map(label).calcValue(child) getOrElse NodeSeq.Empty
      }
    }
  }
  def bindPF(xhtml: NodeSeq)(PF: PartialFunction[Node, RElem]) = {
    def fn(ns: NodeSeq): Seq[RElem] = {
      ns.toSeq map {n =>
        if(PF.isDefinedAt(n))
          PF(n)
        else n match {
          case Elem(pre, label, attributes, scope, child @ _*) =>
            RElem(Elem(pre, label, attributes, scope), child map fn flatten : _*)
          case Text(text) => RElem(text)
          //case Group(nodes) => Group(nodes flatMap fn)
          //case other => other
        }
      }
    }
    fn(xhtml)      
  }
}

trait RElem extends net.liftweb.util.Bindable {
  import scala.ref.WeakReference
  protected var _pages = List[WeakReference[Page]]()
  protected def pages = _pages.flatMap(_.get)
  //protected def observings = pages.flatMap(_.get)
  lazy val id = randomString(20)
  
  def events: Seq[JSEventSource[_<:JSEvent]]
  def properties: Seq[JSProperty[_]]
  
  def baseElem: Elem
  
  protected def addPage(implicit page: Page) {
    // println("In addPage, properties: " + properties)
    if(!_pages.exists(_.get==Some(page))) {
      _pages ::= new WeakReference(page)
      properties.foreach{_ addPage page}
    }
  }
  def render: Elem = {
    println("Rendering " + getClass + "@" + System.identityHashCode(this))
    if(S.request.isEmpty) println("Warning: Rendering even though there is no current request")
    S.request.foreach{_ =>
      // println("Calling addPage")
      addPage(CurrentPage.is)
    }
    val e = baseElem % new UnprefixedAttribute("id", id, Null)
    val withProps = properties.foldLeft(e){
      case (e, prop) => e % prop.asAttribute
    }
    events.foldLeft[Elem](withProps){
      case (e, evt: JSEventSource[_]) => e % evt.asAttribute
      case (e, _) => e
    }
  }
  
  def asHtml = render
}
