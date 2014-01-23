package reactive
package web

import scala.xml._

class NodePredicate(val f: NodeLoc => Boolean)
object NodePredicate {
  implicit def funcPredicate(f: Node => Boolean): NodePredicate = new NodePredicate(f compose (_.node))
  implicit def stringPredicate(s: String): NodePredicate = new NodePredicate(finder(s.head)(s.tail))

  private def finder(c: Char): String => NodeLoc => Boolean = s => n => (c, n) match {
    case ('#', nl                 ) => n.attr.get("id") == Some(s)
    case ('.', nl                 ) => n.classes contains s
    case (':', nl                 ) => n.attr.get("name") == Some(s)
    case ('=', NodeLoc(n, _)      ) => n.text == s
    case ('~', NodeLoc(n, _)      ) => n.text contains s
    case (_,   NodeLoc(e: Elem, _)) => e.label == c + s
    case _                              => false
  }
}

// Based on http://szeiger.de/blog/2009/12/27/a-zipper-for-scala-xml/
case class NavigationException(msg: String) extends RuntimeException(msg)

sealed case class NodeLoc(node: Node, path: NodePath) {
  protected def create(node: Node, path: Hole) = NodeLoc(node, path)
  // Navigation
  final def leftOpt = path match {
    case Hole(tl :: l, p, r)   => Some(create(tl, Hole(l, p, node :: r)))
    case Hole(Nil, _, _) | Top => None
  }
  final def rightOpt = path match {
    case Hole(l, p, tr :: r)   => Some(create(tr, Hole(node :: l, p, r)))
    case Hole(_, _, Nil) | Top => None
  }
  final def downOpt(idx: Int) = {
    val ch = node match {
      case g: Group => g.nodes
      case _        => node.child
    }
    if (ch.isEmpty) None
    else Some(create(ch.head, Hole(ch.tail.take(idx).reverse.toList, this, ch.drop(idx + 1).toList)))
  }
  def upOpt = path match {
    case Hole(l, p, r) =>
      val ns = l.reverse ::: node :: r
      Some(p.setChildren(ns))
    case Top =>
      None
  }
  private def rightOutOpt: Option[NodeLoc] = rightOpt orElse upOpt.flatMap(_.rightOutOpt)
  final def followingOpt = downOpt(0) orElse rightOutOpt
  private def downLastOpt = {
    val ch = node.child
    if (ch.isEmpty) None
    else Some(NodeLoc(ch.head, Hole(ch.reverse.toList.tail, this, Nil)))
  }
  private def downLastTransitiveOpt: Option[NodeLoc] = downLastOpt.flatMap(_.downLastTransitiveOpt)
  final def precedingOpt: Option[NodeLoc] = leftOpt.map(n => n.downLastTransitiveOpt getOrElse n) orElse upOpt

  final def top: NodeLoc = upOpt.map(_.top) getOrElse this

  // Info
  final def isTop = path == Top
  final def depth = path.depth

  // Updates
  final def set(n: Node) = NodeLoc(n, path)

  /**
   * Get a location in which the children of this node are replaced with the given nodes
   * (or throw a NavigationException if isContainer == false)
   */
  final def setChildren(ch: Seq[Node]) = NodeLoc(node match {
    case e: Elem  => e.copy(child = ch)
    case Group(_) => Group(ch)
    case _        => throw NavigationException("Cannot replace children of non-container node " + this);
  }, path)

  def setAttr(name: String, value: Option[String]) = node match {
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
   */
  final def +:(n: Node) = path match {
    case Hole(l, p, r) => NodeLoc(n, Hole(l, p, node :: r))
    case Top           => throw NavigationException("Cannot prepend to the top node")
  }

  /**
   * Insert the given node to the right of this location and return its location
   * (or throw a NavigationException if this location is at the top)
   */
  final def :+(n: Node) = path match {
    case Hole(l, p, r) => NodeLoc(n, Hole(node :: l, p, r))
    case Top           => throw NavigationException("Cannot append to the top node")
  }

  /**
   * Insert the given node to the left of this node's children and return the
   * modified version of this location
   * (or throw a NavigationException is isContainer == false)
   */
  final def prependChild(n: Node) = setChildren(n +: node.child)

  /**
   * Insert the given node to the right of this node's children and return the
   * modified version of this location
   * (or throw a NavigationException is isContainer == false)
   */
  final def appendChild(n: Node) = setChildren(node.child :+ n)

  /**
   * Delete this node. Return the location to the right if it exists,
   * otherwise left if it exists, otherwise up if it exists,
   * otherwise throw a NavigationException.
   */
  final def delete = path match {
    case Hole(l, p, tr :: r) => NodeLoc(tr, Hole(l, p, r))
    case Hole(tl :: l, p, r) => NodeLoc(tl, Hole(l, p, r))
    case Hole(l, p, r) =>
      val list = l.reverse ++ r
      NodeLoc(p.node match {
        case e: Elem  => e.copy(child = list)
        case _: Group => new Group(list)
      }, p.path)
    case _ => throw NavigationException("Cannot delete top node")
  }

  def applyDomMutation(dm: DomMutation) = dm match {
    case DomMutation.InsertChildBefore(parentId, child, beforeId) =>
      child +: (this \\! s"#$parentId" \! s"#$beforeId")
    case DomMutation.AppendChild(parentId, child) =>
      (this \\! s"#$parentId") appendChild child
    case dm@DomMutation.RemoveChild(parentId, oldId) =>
      (this \\! s"#$parentId" \! s"#$oldId").delete
    case DomMutation.ReplaceChild(parentId, child, oldId) =>
      val parent = this \\! s"#$parentId"
      parent setChildren parent.child.map{ c =>
        if (c.id != oldId) c.node
        else child
      }.toSeq
    case DomMutation.ReplaceAll(parentId, child) =>
      this \\! s"#$parentId" setChildren child
    case dm @ DomMutation.UpdateProperty(parentId, _, name, value) =>
      val parent = this \\! s"#$parentId"
      val attr = dm.codec.toAttributeValue(dm.value)(name)
      parent setAttr (name, attr)
  }

  // XPath
  private def stream[A](start: Option[A])(f: A => Option[A]): Stream[A] =
    Stream
      .iterate(start)(_ flatMap f)
      .takeWhile(_.isDefined)
      .map(_.get)

  final def self = Stream(this)

  final def child = stream(downOpt(0))(_.rightOpt)

  final def descendant = child.flatMap(_.descendantOrSelf)

  final def descendantOrSelf: Stream[NodeLoc] = self ++ descendant

  final def parent = upOpt.iterator

  final def ancestor = parent.flatMap(_.ancestorOrSelf)

  final def ancestorOrSelf: Stream[NodeLoc] = self ++ ancestor

  final def followingSibling = stream(rightOpt)(_.rightOpt)

  final def precedingSibling = stream(leftOpt)(_.leftOpt)

  final def following = stream(followingOpt)(_.followingOpt)

  final def preceding = stream(precedingOpt)(_.precedingOpt)

  private def checkPreds(ps: Seq[NodePredicate]): NodeLoc => Boolean =
    nl => ps forall (_.f(nl))

  def \(preds: NodePredicate*): Stream[NodeLoc] =
    child.filter(checkPreds(preds))
  def \?(preds: NodePredicate*): Option[NodeLoc] =
    child.find(checkPreds(preds))
  def \!(preds: NodePredicate*): NodeLoc = \?(preds: _*).get

  def \\(preds: NodePredicate*): Stream[NodeLoc] =
    descendantOrSelf.filter(checkPreds(preds))
  def \\?(preds: NodePredicate*): Option[NodeLoc] =
    descendantOrSelf.find(checkPreds(preds))
  def \\!(preds: NodePredicate*): NodeLoc = \\?(preds: _*).get

  // Attributes
  /**
   * The value of the id attribute
   */
  lazy val id = attr("id")

  /**
   * The value of the class attribute.
   * Note that tests will always use the attribute name
   * of a DomProperty, even if its property name is different.
   */
  lazy val clazz = attr("class")

  /**
   * The css classes, as a Set[String], obtained by
   * splitting clazz and className on whitespace
   */
  lazy val classes: Set[String] =
    attr.get("class").toSet.flatMap{ s: String => s.split("\\s") filter ("" != _) }

  /**
   * The value of the name attribute
   */
  lazy val name = attr("name")

  /**
   * The value of the 'value' attribute
   */
  def value = attr("value")

  /**
   * The attributes, as a Map[String,String]
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
final case object Top extends NodePath {
  val depth = 0
}
final case class Hole(left: List[Node], parent: NodeLoc, right: List[Node]) extends NodePath {
  lazy val depth = parent.depth + 1
}
