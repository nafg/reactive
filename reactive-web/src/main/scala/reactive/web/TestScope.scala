package reactive
package web

import javascript._
import scala.xml.Elem
import scala.xml.NodeSeq
import net.liftweb.json.JsonAST.JValue
import scala.xml.Node
import scala.util.matching.Regex
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.Serialization
import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JBool

/**
 * A scope to simulate the dom mutations that the browser would apply,
 * by directly applying transformations to a NodeSeq
 * @param _xml the initial NodeSeq (such as a processed template)
 */
class TestScope(private var _xml: NodeSeq)(implicit val page: Page) extends LocalScope {
  object logger extends Logger
  case class FiringAjaxEvent(page: Page, event: JValue)

  /**
   * The current xml
   */
  def xml = _xml

  /**
   * All Elems
   */
  def elems: List[Elem] = xml.toList.flatMap(_.descendant_or_self).collect{ case e: Elem => e }

  /**
   * Get a node by id
   */
  def apply(id: String): Node = this / ("#"+id)

  private var confirms: List[(String, Boolean => Unit)] = Nil
  private val ajaxRE = """\(function\(arg0\)\{reactive\.queueAjax\((\d+)\)\(arg0\);reactive.doAjax\(\)\}\)""".r
  private val confirmRE = """window\.confirm\(\"(.*)\"\)""".r

  private object rendered {
    def unapply(a: JsExp[_]) = Some(a.render)
  }

  override def queue[T: CanRender](renderable: T): Unit = {
    renderable match {
      case dm: DomMutation => synchronized {
        _xml = dm(xml)
      }
      case Apply(rendered(ajaxRE(id)), rendered(confirmRE(msg))) => synchronized {
        confirms ::= (msg, b => page.ajaxEvents.fire((id, JBool(b))))
      }
      case _ =>
    }
    super.queue(renderable)
  }

  /**
   * Takes a confirm box handler off the stack and returns
   * it, as an optional pair consisting of the
   * message and a callback function.
  */
  def takeConfirm = synchronized {
    confirms match {
      case hd :: tl =>
        confirms = tl
        Some(hd)
      case Nil =>
        None
    }
  }

  override def toString = s"TestScope(page = $page)"

  class PowerNode(val node: Node) {
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
     * Shorthand for update("value", x).
     * Returns a new node with the updated value
     */
    def value_=[T: PropertyCodec](v: T) = update("value", v)
    /**
     * The attributes, as a Map[String,String]
     */
    lazy val attr = node.attributes.asAttrMap

    /**
     * Update an attribute on this node.
     * The TestScope's xml will be replaced with a copy
     * that's identical except for the attribute replacement.
     * @return the new node (searches the TestScope for a node with the same id).
     */
    def update[T: PropertyCodec](name: String, value: T) = synchronized {
      _xml = DomMutation.UpdateProperty[T](id, name, name, value) apply xml
      TestScope.this(id)
    }

    /**
     * Simulate typing in a text field.
     * Currently only fires KeyUp events, no modifiers, and a Change event.
     */
    def sendKeys(text: String)(implicit page: Page): PowerNode = {
      text.foldLeft(node){
        case (n, '\b') =>
          val v = n.value
          (n.value = v.dropRight(1)) fire KeyUp('\b'.toInt)
        case (n, ch) =>
          (n.value += ch) fire KeyUp(ch.toInt)
      } fire Change()
    }

    /**
     * Simulate an event.
     * Uses regexes to extract the reactive events and property changes that
     * would be fired, and fires them directly.
     * Currently on handles Change, Click, and KeyUp.
     * @return this PowerNode
     */
    def fire[T <: DomEvent](event: T)(implicit page: Page, eventType: Manifest[T]): this.type = {
      for (eventAttr <- attr.get("on"+scalaClassName(eventType.runtimeClass).toLowerCase)) {
        val eventRE = """reactive.eventStreams\[(\d+)\]\.fire\((\{(?:\([^\)]*\)|[^\)])*)\)""".r
        val propRE = """reactive.eventStreams\[(\d+)\]\.fire\(window.document.getElementById\(\"([^\"]*)\"\)\[\"([^\)\"]*)\"\]\)""".r

        val events = (eventRE findAllIn eventAttr).matchData.toList
        val props = (propRE findAllIn eventAttr).matchData.toList

        def replace(replacements: (String, JsExp[_ <: JsTypes.JsAny])*) = { s: String =>
          replacements.foldLeft(s){ case (s, (m, e)) => s.replace(m, JsExp render e) }
        }
        def replaceModifiers: Modifiers => String => String = {
          case Modifiers(alt, ctrl, shift, meta) =>
            replace(
              "event.altKey" -> alt.$,
              "event.ctrlKey" -> ctrl.$,
              "event.shiftKey" -> shift.$,
              "event.metaKey" -> meta.$
            )
        }

        events foreach {
          case Regex.Groups(es, obj) =>
            val eventValue = event match {
              case Change() =>
                obj
              case Click(m) =>
                replaceModifiers(m)(obj)
              case KeyUp(code, m) =>
                replaceModifiers(m)(replace("(event.keyCode||event.charCode)" -> code.$)(obj))
              //TODO other events
            }
            val jvalue = Serialization.read(eventValue)(DefaultFormats, manifest[JValue])
            logger.trace(FiringAjaxEvent(page, jvalue))
            page.ajaxEvents.fire((es, jvalue))
        }
        props foreach {
          case Regex.Groups(es, id, prop) =>
            val e = if (PowerNode.this.id == id) node else TestScope.this(id)
            val jvalue = JString(e.attr(prop))
            logger.trace(FiringAjaxEvent(page, jvalue))
            page.ajaxEvents.fire((es, jvalue))
        }
      }
      this
    }
  }
  implicit def node2powerNode(n: Node): PowerNode = new PowerNode(n)
  implicit def powerNode2node(pn: PowerNode): Node = pn.node

  private def finder(c: Char): String => Node => Boolean = s => c match {
    case '#' => _.attribute("id").map(_.text) == Some(s)
    case '.' => _.classes contains s
    case ':' => _.attribute("name").map(_.text) == Some(s)
    case '=' => _.text == s
    case '~' => _.text contains s
    case _   => _.label == c + s
  }
  implicit def str2pred(s: String): Node => Boolean = finder(s.charAt(0))(s.substring(1))

  class Searchable(ns: Seq[Node]) {
    def /(preds: (Node => Boolean)*): Node =
      ns.flatMap(_.descendant_or_self).find(n => preds.forall(_(n))).get
    def /?(preds: (Node => Boolean)*): Option[Node] =
      ns.flatMap(_.descendant_or_self).find(n => preds.forall(_(n)))
    def /+(preds: (Node => Boolean)*): Seq[Node] =
      ns.flatMap(_.descendant_or_self).filter(n => preds.forall(_(n)))

    def >(preds: (Node => Boolean)*): Node =
      ns.flatMap(_.child).find(n => preds.forall(_(n))).get
    def >?(preds: (Node => Boolean)*): Option[Node] =
      ns.flatMap(_.child).find(n => preds.forall(_(n)))
    def >+(preds: (Node => Boolean)*): Seq[Node] =
      ns.flatMap(_.child).filter(n => preds.forall(_(n)))
  }
  implicit def ns2Searchable(ns: Seq[Node]): Searchable = new Searchable(ns)
  implicit def ts2Searchable(ts: TestScope): Searchable = new Searchable(ts.xml)
}
