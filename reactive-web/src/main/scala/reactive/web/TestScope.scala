package reactive
package web

import javascript._

import scala.xml.Elem
import scala.xml.Node
import scala.util.matching.Regex
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.Serialization
import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JBool


/**
 * A scope to simulate the dom mutations that the browser would apply,
 * by directly applying transformations to a NodeSeq
 * @param _xml the initial NodeSeq (such as a processed template)
 */
class TestScope(_xml: Node)(implicit val page: Page) extends LocalScope {
  object logger extends Logger
  case class FiringAjaxEvent(page: Page, event: JValue)

  private var zipper = NodeLoc(_xml)
  /**
   * The current xml
   */
  def xml = zipper

  /**
   * All Elems
   */
  def elems: List[Elem] = zipper.descendantOrSelf.map(_.node).collect{ case e: Elem => e }.toList

  /**
   * Get a node by id
   */
  def apply(id: String): NodeLoc = zipper \\! ("#" + id)

  private var confirms: List[(String, Boolean => Unit)] = Nil
  private val ajaxRE = """\(function\(arg0\)\{reactive\.queueAjax\((\d+)\)\(arg0\);reactive.doAjax\(\)\}\)""".r
  private val confirmRE = """window\.confirm\(\"(.*)\"\)""".r

  private object rendered {
    def unapply(a: JsExp[_]) = Some(a.render)
  }

  override def queue[T: CanRender](renderable: T): Unit = synchronized {
    renderable match {
      case dm: DomMutation =>
        zipper = zipper.applyDomMutation(dm).top
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

  /**
   * Update an attribute on this node.
   * The TestScope's xml will be replaced with a copy
   * that's identical except for the attribute replacement.
   * @return the new node (searches the TestScope for a node with the same id).
   * @example {{{ testScope(
   */
  def update[T: PropertyCodec](node: NodeLoc, name: String, value: T) = synchronized {
    zipper = zipper.applyDomMutation(DomMutation.UpdateProperty[T](node.id, name, name, value)).top
    TestScope.this(node.id)
  }

  /**
   * Simulate typing in a text field.
   * Currently only fires KeyUp events, no modifiers, and a Change event.
   */
  def sendKeys(node: NodeLoc, text: String)(implicit page: Page): NodeLoc =
    fire(
      text.foldLeft(node){
        case (n, ch) =>
          val v = n.value
          val v2 =
            if (ch == '\b') v.dropRight(1)
            else v + ch
          fire(update(n, "value", v2), KeyUp(ch.toInt))
      },
      Change()
    )

  /**
   * Simulate an event.
   * Uses regexes to extract the reactive events and property changes that
   * would be fired, and fires them directly.
   * Currently only handles Change, Click, and KeyUp.
   * @return the NodeLoc
   */
  def fire[T <: DomEvent](node: NodeLoc, event: T)(implicit page: Page, eventType: Manifest[T]): NodeLoc = {
    for (eventAttr <- node.attr.get("on" + scalaClassName(eventType.runtimeClass).toLowerCase)) {
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
          val e = if (node.id == id) node else TestScope.this(id)
          val jvalue = JString(e.attr(prop))
          logger.trace(FiringAjaxEvent(page, jvalue))
          page.ajaxEvents.fire((es, jvalue))
      }
    }
    node
  }

  override def toString = s"TestScope(page = $page)"
}
