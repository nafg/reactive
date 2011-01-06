package reactive
package web

import scala.xml.NodeSeq

import net.liftweb.http.js.JsCmds.{Run, JsTry, Replace}
import net.liftweb.util.Helpers.stringToSuper

trait RParentElem extends RElem with net.liftweb.http.js.HtmlFixer {
  val children: SeqSignal[RElem]
  
  protected def renderChildren: NodeSeq = children.now.map(_.render)
  override def render = super.render.copy(child = renderChildren)
  
  def handleUpdate(m: Message[RElem,RElem], ids: scala.collection.mutable.Buffer[String]): Unit = m match {
    case Include(index, elem) =>
      println("Received " + m + "; ids == " + ids)
      val e = elem.render
      Reactions.queue(Run(
        "try{var e=document.createElement('" + e.label + "');" + (
          e.attributes.flatMap {attr => ("e.setAttribute('" + attr.key + "'," + attr.value.text.encJs + ");").toCharArray}
        ).mkString + "e.innerHTML = " + fixHtml(elem.id,e.child) + ";" + (
          if(index < ids.length) {
            "document.getElementById('"+this.id+"').insertBefore(e,document.getElementById('"+ids(index)+"'));"
          } else {
            "document.getElementById('"+this.id+"').appendChild(e);"
          }
        ) + "}catch(e){}"
      ))        
      
      ids.insert(index, elem.id)
      
    case Update(index, oldElem, elem) =>
      val e = elem.render
      val oldId = ids(index)
      Reactions.queue(JsTry(Replace(oldId, e),false))
      ids(index) = elem.id
    case Remove(index, oldElem) =>
      println("Received " + m + "; ids == " + ids)
      //TODO
      val oldId = ids(index)
      Reactions queue JsTry(Replace(oldId, NodeSeq.Empty),false)
      ids.remove(index)
    case Batch(ms @ _*) => ms foreach {handleUpdate(_, ids)}
  }
  
  override protected def addPage(implicit page: Page) {
    super.addPage(page)
    val ids = children.now.map(_.id).toBuffer
    children.deltas foreach {m =>
      Reactions.inAnyScope(page){
        handleUpdate(m, ids)
      }
    }
  }
}

