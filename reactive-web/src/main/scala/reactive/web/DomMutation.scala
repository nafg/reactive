package reactive
package web

import javascript._
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Group

import net.liftweb.util.Helpers.encJs
import net.liftweb.http.js.HtmlFixer


object DomMutation extends HtmlFixer {
  private def createElem(id: String, e: Elem): String = {
    "var e=document.createElement('"+e.label+"');"+
      (e.attributes.flatMap { attr =>
        ("e.setAttribute('"+attr.key+"','"+attr.value.text+"');").toCharArray
      }).mkString +
      fixHtmlCmdFunc(id, e.child)("e.innerHTML = "+_+";")
  }

  implicit val defaultDomMutationRenderer: CanRender[DomMutation] = CanRender {
    case InsertChildBefore(parentId, child, prevId) =>
      "try{"+createElem(parentId, child)+
        "var p = document.getElementById('"+parentId+"');"+
        "p.insertBefore(e, document.getElementById('"+prevId+"'))"+
        "}catch(e){}"
    case AppendChild(parentId, child) =>
      "try{"+createElem(parentId, child)+
        "document.getElementById('"+parentId+"').appendChild(e);"+
        "}catch(e){}"
    case RemoveChild(parentId, oldId) =>
      "try{document.getElementById('"+parentId+"').removeChild(document.getElementById('"+oldId+
        "'))}catch(e){}"
    case ReplaceChild(parentId, child, oldId) =>
      "try{"+createElem(parentId, child)+
        "document.getElementById('"+parentId+"').replaceChild(e, document.getElementById('"+oldId+
        "'))}catch(e){}"
    case ReplaceAll(parentId, child) =>
      "try{"+
        fixHtmlCmdFunc(parentId, child)("document.getElementById('"+parentId+"').innerHTML = "+_)+
        "}catch(e){}"
  }

  //TODO should be written with DSL:
  /* 
   * Try {
   *   val e = $ := document.createElement(child.label)
   *   val a = $ := child.attributes.$
   *   ForEach(a) { attr =>
   *     e.setAttribute(attr.key, attr.value.text)
   *   }
   *   e.innerHtml := child.child.toString.$
   *   document.getElementById(parentId) appendChild e
   * } Catch { e =>
   * }
   */

  case class InsertChildBefore(parentId: String, child: Elem, prevId: String) extends DomMutation {
    def updateElem = { e =>
      e.copy(child = xformId(prevId)(e.child)(c => child ++ c))
    }
  }
  case class AppendChild(parentId: String, child: Elem) extends DomMutation {
    def updateElem = { e =>
      e.copy(child = e.child ++ child)
    }
  }
  case class RemoveChild(parentId: String, oldId: String) extends DomMutation {
    def updateElem = { e =>
      e.copy(child = xformId(oldId)(e.child)(_ => NodeSeq.Empty))
    }
  }
  case class ReplaceChild(parentId: String, child: Elem, oldId: String) extends DomMutation {
    def updateElem = { e =>
      e.copy(child = xformId(oldId)(e.child)(_ => child))
    }
  }
  case class ReplaceAll(parentId: String, child: NodeSeq) extends DomMutation {
    def updateElem = _.copy(child = child)
  }
}

sealed trait DomMutation {
  def parentId: String
  protected def updateElem: Elem => Elem
  def xformId(id: String)(in: NodeSeq)(f: Elem => NodeSeq): NodeSeq = in match {
    case e: Elem if e.attribute("id").map(_.text) == Some(parentId) => f(e)
    case Group(ns) => Group(ns flatMap apply)
    case Seq(ns@_*) => ns flatMap apply
    case other => other
  }
  def apply(in: NodeSeq): NodeSeq = xformId(parentId)(in)(updateElem)
}
