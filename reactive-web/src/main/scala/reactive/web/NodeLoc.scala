package reactive
package web

import scala.xml._

/**
 * Wraps a `NodeLoc => Boolean`.
 * Implicit conversions exist that let you encode the predicate in a string (see [[NodePredicate.stringPredicate]])
 * when a `NodePredicate` is needed, or just supply a `scala.xml.Node => Boolean` ([[NodePredicate.funcPredicate]].
 */
class NodePredicate(val f: NodeLoc => Boolean)
object NodePredicate {
  implicit class funcPredicate(f: Node => Boolean) extends NodePredicate(f compose (_.node))

  /**
   * Decodes a node predicate string. The syntax is as follows:
   * {{{
   *   "#id"    -> the node's id must be 'id'
   *   ".class" -> the node's classes must include 'class'
   *   ":name"  -> the node's name must be 'name'
   *   "=text"  -> the node's text content must be 'text'
   *   "~text"  -> the node's text content must contain 'text'
   *   "label"  -> the node's label must be 'label'
   * }}}
   */
  implicit class stringPredicate(s: String) extends NodePredicate(finder(s.head)(s.tail)) {
    override def toString = '"' + s + '"'
  }

  private def finder(c: Char): String => NodeLoc => Boolean = s => nodeLoc => (c, nodeLoc) match {
    case ('#', _)                   => nodeLoc.attr.get("id").contains(s)
    case ('.', _)                   => nodeLoc.classes contains s
    case (':', _)                   => nodeLoc.attr.get("name").contains(s)
    case ('=', NodeLoc(n, _)      ) => n.text == s
    case ('~', NodeLoc(n, _)      ) => n.text contains s
    case (_,   NodeLoc(e: Elem, _)) => e.label == c + s
    case _                          => false
  }
}

case class NavigationException(msg: String) extends RuntimeException(msg)

/**
 * This class implements an XML zipper.
 * @see [[NodeLoc.apply]]
 * @see [[http://szeiger.de/blog/2009/12/27/a-zipper-for-scala-xml/]]
 * @groupname nav Zipper navigation
 * @constructor
 * @param node The underlying `scala.xml.Node`
 */
sealed case class NodeLoc(node: Node, path: NodePath) {
  protected def create(node: Node, path: Hole): NodeLoc = NodeLoc(node, path)

  /** @group nav */
  final def leftOpt: Option[NodeLoc] = path match {
    case Hole(tl :: l, p, r)   => Some(create(tl, Hole(l, p, node :: r)))
    case Hole(Nil, _, _) | Top => None
  }
  /** @group nav */
  final def rightOpt = path match {
    case Hole(l, p, tr :: r)   => Some(create(tr, Hole(node :: l, p, r)))
    case Hole(_, _, Nil) | Top => None
  }
  /** @group nav */
  final def downOpt(idx: Int) = {
    val ch = node match {
      case g: Group => g.nodes
      case _        => node.child
    }
    if (ch.isEmpty) None
    else Some(create(ch.head, Hole(ch.tail.take(idx).reverse.toList, this, ch.drop(idx + 1).toList)))
  }
  /** @group nav */
  def upOpt = path match {
    case Hole(l, p, r) =>
      val ns = l.reverse ::: node :: r
      Some(p.setChildren(ns))
    case Top =>
      None
  }
  /** @group nav */
  private def rightOutOpt: Option[NodeLoc] = rightOpt orElse upOpt.flatMap(_.rightOutOpt)
  /** @group nav */
  final def followingOpt = downOpt(0) orElse rightOutOpt
  /** @group nav */
  private def downLastOpt = {
    val ch = node.child
    if (ch.isEmpty) None
    else Some(NodeLoc(ch.head, Hole(ch.reverse.toList.tail, this, Nil)))
  }
  /** @group nav */
  private def downLastTransitiveOpt: Option[NodeLoc] = downLastOpt.flatMap(_.downLastTransitiveOpt)
  /** @group nav */
  final def precedingOpt: Option[NodeLoc] = leftOpt.map(n => n.downLastTransitiveOpt getOrElse n) orElse upOpt

  /** @group nav */
  final def top: NodeLoc = upOpt.map(_.top) getOrElse this

  /** @group Info */
  final def isTop = path == Top
  /** @group Info */
  final def depth = path.depth

  /**
   * Replaces the `Node`
   * @group Updates
   */
  final def set(n: Node): NodeLoc = NodeLoc(n, path)

  /**
   * Replace the children of this node with the given nodes
   * (or throw a NavigationException if isContainer == false)
   * @group Updates
   */
  final def setChildren(ch: Seq[Node]) = NodeLoc(node match {
    case e: Elem  => e.copy(child = ch)
    case Group(_) => Group(ch)
    case _        => throw NavigationException("Cannot replace children of non-container node " + this);
  }, path)

  /**
   * Set or clear an attribute
   * (or throw a NavigationException if the node is not an `Elem`)
   * @group Updates
   */
  def setAttr(name: String, value: Option[String]): NodeLoc = node match {
    case e: Elem =>
      val e2 = value match {
        case Some(v) =>
          e % new scala.xml.UnprefixedAttribute(name, v, scala.xml.Null)
        case None =>
          e.copy(attributes = e.attributes.filter(_.key != name))
      }
      set(e2)
    case _       => throw NavigationException("Can only set attributes of Elem")
  }

  /**
   * Insert the given node to the left of this location and return its location
   * (or throw a NavigationException if this location is at the top)
   * @group Updates
   */
  final def +:(n: Node) = path match {
    case Hole(l, p, r) => NodeLoc(n, Hole(l, p, node :: r))
    case Top           => throw NavigationException("Cannot prepend to the top node")
  }

  /**
   * Insert the given node to the right of this location and return its location
   * (or throw a NavigationException if this location is at the top)
   * @group Updates
   */
  final def :+(n: Node) = path match {
    case Hole(l, p, r) => NodeLoc(n, Hole(node :: l, p, r))
    case Top           => throw NavigationException("Cannot append to the top node")
  }

  /**
   * Insert the given node to the left of this node's children and return the
   * modified version of this location
   * (or throw a NavigationException is isContainer == false)
   * @group Updates
   */
  final def prependChild(n: Node) = setChildren(n +: node.child)

  /**
   * Insert the given node to the right of this node's children and return the
   * modified version of this location
   * (or throw a NavigationException is isContainer == false)
   * @group Updates
   */
  final def appendChild(n: Node) = setChildren(node.child :+ n)

  /**
   * Delete this node. Return the location to the right if it exists,
   * otherwise left if it exists, otherwise up if it exists,
   * otherwise throw a NavigationException.
   * @group Updates
   */
  final def delete = path match {
    case Hole(l, p, tr :: r) => NodeLoc(tr, Hole(l, p, r))
    case Hole(tl :: l, p, r) => NodeLoc(tl, Hole(l, p, r))
    case Hole(l, p, r) =>
      val list = l.reverse ++ r
      NodeLoc(p.node match {
        case e: Elem  => e.copy(child = list)
        case _: Group => Group(list)
      }, p.path)
    case _ => throw NavigationException("Cannot delete top node")
  }

  /**
    * Modify the tree by applying a [[DomMutation]]
    *
    * @group Updates
    */
  def applyDomMutation(dm: DomMutation) = dm match {
    case DomMutation.InsertChildBefore(parentId, child, beforeId) =>
      child +: (this \\! s"#$parentId" \! s"#$beforeId")
    case DomMutation.AppendChild(parentId, child)                 =>
      (this \\! s"#$parentId") appendChild child
    case DomMutation.RemoveChild(parentId, oldId)                 =>
      (this \\! s"#$parentId" \! s"#$oldId").delete
    case DomMutation.ReplaceChild(parentId, child, oldId)         =>
      val parent = this \\! s"#$parentId"
      parent setChildren parent.child.map { c =>
        if (c.id != oldId) c.node
        else child
      }
    case DomMutation.ReplaceAll(parentId, child)                  =>
      this \\! s"#$parentId" setChildren child
    case dm@DomMutation.UpdateProperty(parentId, _, attrName, _)  =>
      val parent = this \\! s"#$parentId"
      val attr = dm.codec.toAttributeValue(dm.value)(attrName)
      parent setAttr(attrName, attr)
  }

  /** @group XPath */
  private def stream[A](start: Option[A])(f: A => Option[A]): Stream[A] =
    Stream
      .iterate(start)(_ flatMap f)
      .takeWhile(_.isDefined)
      .map(_.get)

  /** @group XPath */
  final def self = Stream(this)

  /** @group XPath */
  final def child = stream(downOpt(0))(_.rightOpt)

  /** @group XPath */
  final def descendant = child.flatMap(_.descendantOrSelf)

  /** @group XPath */
  final def descendantOrSelf: Stream[NodeLoc] = self ++ descendant

  /** @group XPath */
  final def parent = upOpt.iterator

  /** @group XPath */
  final def ancestor = parent.flatMap(_.ancestorOrSelf)

  /** @group XPath */
  final def ancestorOrSelf: Stream[NodeLoc] = self ++ ancestor

  /** @group XPath */
  final def followingSibling = stream(rightOpt)(_.rightOpt)

  /** @group XPath */
  final def precedingSibling = stream(leftOpt)(_.leftOpt)

  /** @group XPath */
  final def following = stream(followingOpt)(_.followingOpt)

  /** @group XPath */
  final def preceding = stream(precedingOpt)(_.precedingOpt)

  /** @group XPath */
  private def checkPreds(ps: Seq[NodePredicate]): NodeLoc => Boolean =
    nl => ps forall (_.f(nl))

  /**
   * Returns a `Stream` of direct children matching the [[NodePredicate]]
   * @group XPath
   */
  def \(preds: NodePredicate*): Stream[NodeLoc] =
    child.filter(checkPreds(preds))
  /**
   * Returns a direct child matching the [[NodePredicate]]
   * if one exists, in an `Option`
   * @group XPath
   */
  def \?(preds: NodePredicate*): Option[NodeLoc] =
    child.find(checkPreds(preds))
  /**
   * Returns a direct child matching the [[NodePredicate]].
   * It is an error if none exists.
   * @group XPath
   */
  def \!(preds: NodePredicate*): NodeLoc = \?(preds: _*).getOrElse(sys.error("Could not find any node with predicates "+preds.mkString(", ")))

  /**
   * Returns a `Stream` of all descendants matching the [[NodePredicate]]
   * @group XPath
   */
  def \\(preds: NodePredicate*): Stream[NodeLoc] =
    descendantOrSelf.filter(checkPreds(preds))
  /**
   * Returns a descendant matching the [[NodePredicate]]
   * if one exists, in an `Option`
   * @group XPath
   */
  def \\?(preds: NodePredicate*): Option[NodeLoc] =
    descendantOrSelf.find(checkPreds(preds))
  /**
   * Returns a descendant matching the [[NodePredicate]].
   * It is an error if none exists.
   * @group XPath
   */
  def \\!(preds: NodePredicate*): NodeLoc = \\?(preds: _*).getOrElse(sys.error("Could not find any node with predicates "+preds.mkString(", ")))

  // Attributes

  /**
   * The value of the id attribute
   * @note Convenience shortcut for `attr("id")`
   * @group Attributes
   */
  lazy val id = attr("id")

  /**
   * The value of the class attribute.
   * Note that tests will always use the attribute name
   * of a DomProperty, even if its property name is different.
   * @note Convenience shortcut for `attr("class")`
   * @group Attributes
   */
  lazy val clazz = attr("class")

  /**
   * The css classes, as a Set[String], obtained by
   * splitting clazz and className on whitespace
   * @group Attributes
   */
  lazy val classes: Set[String] =
    attr.get("class").toSet.flatMap{ s: String => s.split("\\s") filter ("" != _) }

  /**
   * The value of the "name" attribute
   * @note Convenience shortcut for `attr("name")`
   * @group Attributes
   */
  lazy val name = attr("name")

  /**
   * The value of the "value" attribute.0
   * @note Convenience shortcut for `attr("value")`
   * @group Attributes
   */
  def value: String = attr("value")

  /**
   * Set the value of the "value" attribute
   * @return a new `NodeLoc` with the attribute set
   * @note Convenience shortcut for `setAttr("value", Some(s))`
   * @group Attributes
   */
  def value_=(s: String): NodeLoc = setAttr("value", Some(s))

  /**
   * The attributes, as a Map[String,String]
   * @group Attributes
   */
  lazy val attr = node match {
    case e: Elem => e.attributes.asAttrMap
    case _       => Map.empty[String, String]
  }
}

object NodeLoc {
  def apply(node: Node): NodeLoc = new CachedTopNodeLoc(node)
}

final class CachedParentNodeLoc(node: Node, path: Hole) extends NodeLoc(node, path) {
  override def upOpt = Some(path.parent)
  override protected def create(node: Node, path: Hole) = new CachedParentNodeLoc(node, path)
}
final class CachedTopNodeLoc(node: Node) extends NodeLoc(node, Top) {
  override def upOpt = None
  override protected def create(node: Node, path: Hole) = new CachedParentNodeLoc(node, path)
}

sealed trait NodePath {
  def depth: Int
}

case object Top extends NodePath {
  val depth = 0
}
final case class Hole(left: List[Node], parent: NodeLoc, right: List[Node]) extends NodePath {
  lazy val depth = parent.depth + 1
}
