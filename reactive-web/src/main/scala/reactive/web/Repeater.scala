package reactive
package web

import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.js.{JsCmd, JsCmds, HtmlFixer}
  import JsCmds.{Run, JsTry, Replace}
import net.liftweb.util.Helpers.stringToSuper

class RepeaterManager(parentId: String, children: SeqSignal[RElem]) extends HtmlFixer {
  def handleUpdate(m: Message[RElem,RElem], ids: scala.collection.mutable.Buffer[String]): JsCmd = m match {
    case Include(index, elem) =>
      val e = elem.render
      ids.insert(index, elem.id)
      Run(
        "try{var e=document.createElement('" + e.label + "');" + (
          e.attributes.flatMap {attr => ("e.setAttribute('" + attr.key + "'," + attr.value.text.encJs + ");").toCharArray}
        ).mkString + "e.innerHTML = " + fixHtml(elem.id,e.child) + ";" + (
          if(index < ids.length - 1) {
            "document.getElementById('"+parentId+"').insertBefore(e,document.getElementById('"+ids(index)+"'));"
          } else {
            "document.getElementById('"+parentId+"').appendChild(e);"
          }
        ) + "}catch(e){}"
      )      
      
    case Update(index, oldElem, elem) =>
      val e = elem.render
      val oldId = ids(index)
      ids(index) = elem.id
      JsTry(Replace(oldId, e),false)
      
    case Remove(index, oldElem) =>
      JsTry(Replace(ids.remove(index), NodeSeq.Empty),false)
      
    case Batch(ms @ _*) =>
      ms.map(handleUpdate(_, ids)).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
  }
  
  def createPageStream: EventStream[JsCmd] = {
    val ids = children.now.map(_.id).toBuffer
    children.deltas map {m => handleUpdate(m, ids)}
  }
}

trait Repeater extends RElem with HtmlFixer {
  
  def children: SeqSignal[RElem]
  
  protected def renderChildren: NodeSeq = children.now.map(_.render)
  
  override def render = super.render.copy(child = renderChildren)
  
  private lazy val manager = new RepeaterManager(this.id, children)
  
  override protected def addPage(implicit page: Page) {
    super.addPage(page)
    manager.createPageStream foreach { js =>
      Reactions.inAnyScope(page) {
        Reactions.queue(js)
      }
    }
  }
}


object Repeater {
  def apply(binding: SeqSignal[NodeSeq=>NodeSeq]) = {ns: NodeSeq =>
    new Repeater {
      val baseElem = nodeSeqToElem(ns)
      val events, properties = Nil
      lazy val children = binding map {
        _ map {f => RElem(nodeSeqToElem(f(baseElem.child)))}
      } 
    }.render
  }
}

