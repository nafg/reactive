package reactive
package web

import javascript._
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Group
import net.liftweb.util.Helpers.encJs
import net.liftweb.http.js.HtmlFixer

object DomMutation extends HtmlFixer {
  private def createElem(id: String, e: Elem)(f: String => String): String =
    fixHtmlCmdFunc(id, e.child)("reactive.createElem('%s',%s,%s)".format(
      e.label,
      e.attributes.map{ md => "'"+md.key+"':'"+md.value.text+"'" }.mkString("{", ",", "}"),
      _
    )
    )
  val defaultDomMutationRenderer: CanRender[DomMutation] = CanRender {
    case InsertChildBefore(parentId, child, prevId) =>
      createElem(parentId, child)("reactive.insertChild('%s',%s,'%s')".format(parentId, _, prevId))
    case AppendChild(parentId, child) =>
      createElem(parentId, child)("reactive.appendChild('%s',%s)".format(parentId, _))
    case RemoveChild(parentId, oldId) =>
      "reactive.removeChild('%s','%s')".format(parentId, oldId)
    case ReplaceChild(parentId, child, oldId) =>
      createElem(parentId, child)("reactive.replaceChild('%s',%s,'%s')".format(parentId, _, oldId))
    case ReplaceAll(parentId, child) =>
      fixHtmlCmdFunc(parentId, child)("reactive.replaceAll('%s',%s)".format(parentId, _))
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
    case e: Elem if e.attribute("id").map(_.text) == Some(id) =>
      f(e)
    case Group(ns) =>
      Group(ns flatMap apply)
    case s@Seq(ns@_*) =>
      if (ns.length > 1)
        ns flatMap apply
      else if (ns.length == 1 && (s ne ns.head))
        apply(ns.head)
      else
        ns
    case other =>
      other
  }
  def apply(in: NodeSeq): NodeSeq = xformId(parentId)(in)(updateElem)
}
