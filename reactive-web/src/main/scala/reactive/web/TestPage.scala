package reactive
package web

import javascript._

import scala.xml._
import scala.util.matching.Regex
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.Serialization
import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JBool

import reactive.logging.HasLogger

/**
 * A [[Page]] for testing purposes, that
 * simulates a browser's html state.
 * @param initialXml An optional function that builds
 * the initial x/html state. The `TestPage` instance
 * is passed to the function, in case it's needed to
 * build the html.
 */
class TestTransportType(page: Page, initialXml: =>Node = Group(Nil)) extends TransportType with HasLogger {
  case class FiringAjaxEvent(page: Page, event: JValue)

  private lazy val zipper = new AtomicRef[NodeLoc](NodeLoc(initialXml))

  private var confirms: List[(String, Boolean => Unit)] = Nil

  private object xmlLazyLock
  /**
   * The current xml
   */
  def xml = zipper.get

  /**
   * All Elems
   */
  def elems: List[Elem] = xml.descendantOrSelf.map(_.node).collect{ case e: Elem => e }.toList

  /**
   * Get a node by id
   */
  def apply(id: String): NodeLoc = xml \\! ("#" + id)

  def transform(f: NodeLoc => NodeLoc) = zipper transform (f andThen (_.top))

  def update(zip: NodeLoc) = zipper transform (_ => zip.top)

  /**
   * Update an attribute on this node.
   * The testTransport's xml will be replaced with a copy
   * that's identical except for the attribute replacement.
   * @return the new node (searches the testTransport for a node with the same id).
   * @example {{{ testTransport(testTransport.xml \\ "#Id", "value") = "newValue" }}}
   */
  def update[T: PropertyCodec](node: NodeLoc, name: String, value: T) = zipper.run{ nl =>
    val newNL = nl.applyDomMutation(DomMutation.UpdateProperty[T](node.id, name, name, value)).top
    (newNL, newNL \\! s"#${node.id}")
  }

  /**
   * A transport that simulates the dom mutations that a browser would apply,
   * by directly transforming a NodeSeq representing the state of
   * the html that would be in the browser
   * @param _xml the initial NodeSeq (such as a rendered template)
   */
  object testTransport extends AccumulatingTransport {
    def currentPriority = 1000

    private val queuingNow = new scala.util.DynamicVariable(Vector.empty[Renderable])

    private val ajaxRE = ("""\(function\(arg0\)\{reactive\.queueAjax\((\d+)\)\(arg0\);reactive.doAjax\('"""+page.id+"""'\)\}\)""").r
    private val confirmRE = """window\.confirm\(\"(.*)\"\)""".r

    private object rendered {
      def unapply(a: JsExp[_]) = Some(a.render)
    }

    queued foreach { renderable =>
      synchronized {
        renderable match {
          case dmr @ DomMutationRenderable(dm) =>
            try transform(_.applyDomMutation(dm))
            catch {
              case e: Exception =>
                Console.err.println("TestPage: Could not apply DomMutation " + dm)
            }
          case JavascriptStatementRenderable(Apply(rendered(ajaxRE(id)), rendered(confirmRE(msg)))) =>
            confirms ::= (msg, b => ajaxEvents.fire((id, JBool(b))))
          case _ =>
        }
        queuingNow.value :+= renderable
      }
    }

    def collectQueued(f: =>Unit): Seq[Renderable] = {
      queuingNow.withValue(Vector.empty) {
        f
        queuingNow.value
      }
    }

    override def toString = s"testTransport(page = $page)"
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
   * Simulate typing in a text field.
   * Currently only fires KeyUp events, no modifiers, and a Change event.
   */
  def sendKeys(node: NodeLoc, text: String): NodeLoc =
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
  def fire[T <: DomEvent](node: NodeLoc, event: T)(implicit eventType: Manifest[T]): NodeLoc = {
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
          ajaxEvents.fire((es, jvalue))
      }
      props foreach {
        case Regex.Groups(es, id, prop) =>
          val e = if (node.id == id) node else apply(id)
          val jvalue = JString(e.attr.getOrElse(prop, ""))
          logger.trace(FiringAjaxEvent(page, jvalue))
          ajaxEvents.fire((es, jvalue))
      }
    }
    node
  }

  /**
   * Run a block of code
   * and return the javascript that got queued to this page
   * during the course of running the block
   */
  def collectQueued(f: =>Unit): Seq[Renderable] = testTransport.collectQueued(f)

  linkTransport(testTransport)
}

class TestPage(initialXml: TestPage => Node = _ => Group(Nil)) extends Page {
  lazy val initial = initialXml(this)

  object testTransportType extends TestTransportType(this, initial)
  val transportTypes = testTransportType :: Nil

  testTransportType.xml

  def collectQueued(f: =>Unit): Seq[Renderable] = testTransportType.collectQueued(f)
}
