package reactive
package web

import javascript._
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Group

import net.liftweb.util.Helpers.encJs
import net.liftweb.http.js.HtmlFixer

object DomMutation extends HtmlFixer {
  private def elemString(id: String, ns: NodeSeq) = fixHtmlCmdFunc(id, ns)(s => s)
  private def createElem(id: String, e: Elem): String = "reactive.createElem('%s',%s,%s)".format(
    e.label,
    e.attributes.map{ md => "'"+md.key+"':'"+md.value.text+"'" }.mkString("{", ",", "}"),
    elemString(id, e.child)
  )
  val defaultDomMutationRenderer: CanRender[DomMutation] = CanRender {
    case InsertChildBefore(parentId, child, prevId) =>
      "reactive.insertChild('%s',%s,'%s')".format(parentId, createElem(parentId, child), prevId)
    case AppendChild(parentId, child) =>
      "reactive.appendChild('%s',%s)".format(parentId, createElem(parentId, child))
    case RemoveChild(parentId, oldId) =>
      "reactive.removeChild('%s','%s')".format(parentId, oldId)
    case ReplaceChild(parentId, child, oldId) =>
      "reactive.replaceChild('%s',%s,'%s')".format(parentId, createElem(parentId, child), oldId)
    case ReplaceAll(parentId, child) =>
      "reactive.replaceAll('%s',%s)".format(parentId, elemString(parentId, child))
  }

  //TODO should be written with DSL: JsStub for reactive object

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
