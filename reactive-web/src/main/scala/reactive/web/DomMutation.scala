package reactive
package web

import javascript._
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Group
import net.liftweb.util.Helpers.encJs
import net.liftweb.http.js.HtmlFixer
import scala.xml.Node
import scala.xml.UnprefixedAttribute
import scala.xml.Null

object DomMutation extends HtmlFixer {
  private def createElem(id: String, e: Elem)(f: String => String): String =
    fixHtmlCmdFunc(id, e.child){ s =>
      f(
        "reactive.createElem('%s',%s,%s)".format(
          e.label,
          e.attributes.map{ md => "'"+md.key+"':'"+md.value.text+"'" }.mkString("{", ",", "}"),
          s
        )
      )
    }
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
    case up@UpdateProperty(parentId, pname, aname, v) =>
      "reactive.updateProperty('%s','%s',%s)".format(parentId, pname, JsExp render up.codec.toJS(v))
  }

  //TODO should be written with DSL: JsStub for reactive object

  case class InsertChildBefore(parentId: String, child: Elem, beforeId: String) extends DomMutation {
    def updateElem = { e =>
      e.copy(child = xformId(beforeId)(c => child ++ c)(e.child))
    }
  }
  case class AppendChild(parentId: String, child: Elem) extends DomMutation {
    def updateElem = { e =>
      e.copy(child = e.child ++ child)
    }
  }
  case class RemoveChild(parentId: String, oldId: String) extends DomMutation {
    def updateElem = { e =>
      e.copy(child = xformId(oldId)(_ => NodeSeq.Empty)(e.child))
    }
  }
  case class ReplaceChild(parentId: String, child: Elem, oldId: String) extends DomMutation {
    def updateElem = { e =>
      e.copy(child = xformId(oldId)(_ => child)(e.child))
    }
  }
  case class ReplaceAll(parentId: String, child: NodeSeq) extends DomMutation {
    def updateElem = _.copy(child = child)
  }
  case class UpdateProperty[T](parentId: String, propertyName: String, attributeName: String, value: T)(implicit val codec: PropertyCodec[T]) extends DomMutation {
    def updateElem = { e =>
      codec.toAttributeValue(value)(attributeName).map(e % new UnprefixedAttribute(attributeName, _, Null)).getOrElse(e)
    }
  }
}

sealed trait DomMutation {
  def parentId: String
  protected def updateElem: Elem => Elem
  /**
   * Returns a NodeSeq=>NodeSeq function that traverses all nodes,
   * and applies a function f to the Elem with the specified id.
   * If multiple nodes with the id exist, behavior is undefined.
   */
  def xformId(id: String)(f: Elem => NodeSeq): NodeSeq => NodeSeq = {
    case e: Elem =>
      if (e.attribute("id").map(_.text) == Some(id))
        f(e)
      else
        e.copy(child = e.child flatMap xformId(id)(f))
    case Group(ns) =>
      Group(ns flatMap xformId(id)(f))
    case s@Seq(ns@_*) =>
      if (ns.length > 1)
        ns flatMap xformId(id)(f)
      else if (ns.length == 1 && (s ne ns.head))
        xformId(id)(f)(ns.head)
      else
        ns
  }
  def apply(in: NodeSeq): NodeSeq = xformId(parentId)(updateElem)(in)
}
