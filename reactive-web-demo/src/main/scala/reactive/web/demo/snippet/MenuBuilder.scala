package reactive.web
package demo.snippet

import net.liftweb.sitemap.FlexMenuBuilder
import scala.xml.NodeSeq
import net.liftweb.util.Helpers
import scala.xml.Elem
import scala.xml.Group
import scala.xml.Text

//object TopMenuBuilder extends FlexMenuBuilder {
//  override def expandAny = false
//  override def linkToSelf = true
//}

object SideMenuBuilder extends FlexMenuBuilder {
  override def expandAll = true

  override def linkToSelf = true

  override def renderOuterTag(inner: NodeSeq, top: Boolean) = {
    val nav = super.renderOuterTag(inner, top) match {
      case e: Elem => Helpers.addCssClass("nav nav-list", e)
    }
    if (!top) nav
    else {
      nav.copy(child = nav.child.map {
        case e: Elem =>
          val childElems = e.child.flatMap {
            case g: Group => g.nodes.collect { case e: Elem => e }
            case e: Elem  => Seq(e)
            case _        => Nil
          }
          val out = childElems match {
            case Seq(<span>{ header }</span>, <ul>{ lis @ _* }</ul>) =>
              <li class="nav-header">{ header }</li> ++ lis
            case other => other
          }
          e.copy(child = out)
        case other => other
      })
    }
  }

  override def updateForCurrent(elem: Elem, current: Boolean): Elem = if (!current) elem else Helpers.addCssClass("active", elem)
}
