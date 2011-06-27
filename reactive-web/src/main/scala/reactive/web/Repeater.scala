package reactive
package web

import scala.xml.{ Elem, NodeSeq }

import net.liftweb.http.js.{ JsCmd, JsCmds, HtmlFixer }
import JsCmds.{ Run, JsTry, Replace }
import net.liftweb.util.Helpers.stringToSuper

/**
 * This class is used internally by Repeater. It generates javascript to add and remove children from a DOM element.
 * @param parentId the id to insert and remove children from
 * @param children the RElems to be contained by the element with id parentId
 */
class RepeaterManager(children: SeqSignal[RElem]) extends HtmlFixer {
  /**
   * Given an incremental update and the current set of children's ids, returns a JsCmd to apply the delta.
   * @param m the delta
   * @param ids the current ids of the child elements, in order
   * @return a JsCmd that when executed by the browser will cause update the DOM to reflect the change represented by the delta.
   */
  def handleUpdate(
    parentId: String,
    m: SeqDelta[RElem, RElem],
    ids: Seq[String])(implicit p: Page): (Seq[String], JsCmd) = m match {
    case Include(index, elem) =>
      val e = elem.render
      ids.patch(index, Seq(elem.id), 0) ->
        Run(
          "try{var e=document.createElement('"+e.label+"');"+
            (e.attributes.flatMap { attr =>
              ("e.setAttribute('"+attr.key+"',"+attr.value.text.encJs+");").toCharArray
            }).mkString+"e.innerHTML = "+fixHtml(elem.id, e.child)+";"+
            (if (index < ids.length) {
              "document.getElementById('"+parentId+"').insertBefore(e,document.getElementById('"+elem.id+"'));"
            } else {
              "document.getElementById('"+parentId+"').appendChild(e);"
            })+"}catch(e){}"
        )

    case Update(index, oldElem, elem) =>
      val e = elem.render
      val oldId = ids(index)
      ids.patch(index, Seq(elem.id), 1) ->
        JsTry(Replace(oldId, e), false)

    case Remove(index, _) =>
      ids.patch(index, Nil, 1) ->
        Run(
          "try{document.getElementById('"+parentId+"').removeChild(document.getElementById('"+ids(index)+
            "'))}catch(e){}"
        )

    case Batch(ms @ _*) =>
      ms.foldLeft[(Seq[String],JsCmd)]((ids,JsCmds.Noop)){case ((ids,cmds),delta) =>
        handleUpdate(parentId,delta,ids) match {
          case (ids, cmd) => (ids, cmds & cmd)
        }
      }
  }

  /**
   * Returns an EventStream that will fire a JsCmd whenever children changes
   */
  def createPageStream(parentId: String)(implicit p: Page): EventStream[JsCmd] = {
    children.deltas.foldLeft((children.now.map(_.id(p)): Seq[String],JsCmds.Noop: JsCmd)){case ((ids,cmds),deltas) =>
      handleUpdate(parentId, deltas, ids)
    }.map(_._2)
  }
}

/**
 * Allows one to render a SeqSignal[RElem] inside an RElem.
 * When items are added or removed from the SeqSignal, they are added and removed from the browser DOM.
 * The multiple-child counterpart of Cell.
 */
trait Repeater extends RElem with HtmlFixer {
  /**
   * The SeqSignal[RElem] to render and keep up to date in the browser
   */
  def children: SeqSignal[RElem]

  protected def renderChildren(implicit p: Page): NodeSeq = children.now.map(_.render)

  override def renderer(implicit p: Page) = e => super.renderer(p)(e).copy(child = renderChildren)

  private lazy val manager = new RepeaterManager(children)

  override protected def addPage(elem: Elem)(implicit page: Page): Elem = {
    val ret = super.addPage(elem)(page)
    manager.createPageStream(id(page)) foreach { js =>
      Reactions.inAnyScope(page) {
        Reactions.queue(js)
      }
    }
    ret
  }
}

/**
 * Provides a factory for creating Repeaters
 */
object Repeater {
  /**
   * Returns a Lift binding function that will render a Repeater, using the element
   * from the template as the containing element.
   * The children of that element will be passed to each element in the SeqSignal,
   * to render that element.
   * @param binding a SeqSignal where each element is a binding function that renders one element in the view
   */
  def apply(binding: SeqSignal[_ <: NodeSeq => NodeSeq])(implicit p: Page): NodeSeq => NodeSeq = { ns: NodeSeq =>
    new Repeater {
      val baseElem = nodeSeqToElem(ns)
      val events, properties = Nil
      lazy val children = binding map {
        _ map { f => RElem(nodeSeqToElem(f(baseElem.child))) }
      }
    }.render
  }
}

