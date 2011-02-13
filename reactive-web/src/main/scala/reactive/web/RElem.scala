package reactive
package web



import net.liftweb.http._
  import js._
    import JsCmds._
    import JE._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.actor._
import scala.xml._



/**
 * This singleton provides some useful things, including factories for creating RElems from standard Scala types.
 */
object RElem {
  /**
   * An RElem based on a scala.xml.Elem.
   * @param parent the Elem to use. If it already has an id, it is the programmer's responsibility to ensure it is unique
   * @param children any addition RElems to append
   */
  class ElemWrapper(parent: Elem, val children: RElem*) extends RElem {
    val (baseElem, _id) = parent.attributes.get("id") match {
      case Some(id) =>
        (parent, id.text)
      case None =>
        val id = randomString(20)
        (parent % new UnprefixedAttribute("id", id, Null), id)
    }
    override lazy val id = _id
    val properties, events = Nil
    override def render = baseElem.copy(child = baseElem.child ++ children.map(_.render))
  }
  private[reactive] val elems = new scala.collection.mutable.WeakHashMap[String,RElem]
  
  /**
   * Wraps a Scala String=>Unit function in a Lift AFuncHolder that
   * runs the provided function in the client scope. Exceptions are intercepted.
   */
  def ajaxFunc(f: String=>Unit): S.AFuncHolder = S.LFuncHolder{
    case Nil => JsCmds.Noop
    case s :: _ => Reactions.inClientScope {
      try {
        f(s)
      } catch {
        case e: Exception =>
          e.printStackTrace
          JsCmds.Noop
      }
    }
  }
  
  /**
   * Creates an RElem from the given scala.xml.Elem. One may provide 0 or more RElems to append.
   * @param parent the Elem to use. If it has an id you must ensure it is unique
   * @param children any children to append
   */
  def apply(parent: Elem, children: RElem*): RElem = new ElemWrapper(parent, children: _*)
  /**
   * Creates an RElem from a text String, wrapping it in a span
   */
  def apply(text: String): RElem = new ElemWrapper(<span>{text}</span>)

}

/**
 * The base trait of all reactive elements.
 * Has the ability to be rendered. Rendering will generate an Elem that
 * contains attributes representing the current state of properties,
 * and attributes with event handlers. Note that you must add listeners
 * to events before it is rendered; otherwise the attribute may not be
 * generated. DOM events will be sent to the server and appear as an
 * EventStream.
 * In addition properties can be kept in sync with the browser in
 * response to events, and mutating them causes the DOM to be updated
 * in the browser.
 */
trait RElem extends net.liftweb.util.Bindable {
  import scala.ref.WeakReference
  protected var _pages = List[WeakReference[Page]]()
  /**
   * Which Pages this RElem has been rendered to.
   * It will be kept in sync on all of them.
   */
  protected def pages = _pages.flatMap(_.get)
  
  /**
   * The value of the id attribute 
   */
  lazy val id = randomString(20)
  
  /**
   * The events that contribute to rendering
   */
  def events: Seq[DOMEventSource[_<:DOMEvent]]
  /**
   * The properties that contribute to rendering
   */
  def properties: Seq[DOMProperty[_]]
  
  /**
   * The Elem used as the basis to render this RElem.
   * The final rendering is contributed to by events
   * and properties as well.
   */
  def baseElem: Elem
  
  /**
   * Called (from render) to register a Page with this
   * RElem.
   * Pages are used (a) as an Observing to manage listener
   * references; (b) to link server-context updates
   * with the right comet actor; and (c) to allow
   * the same element state to be maintained on
   * multiple pages. 
   */
  protected def addPage(implicit page: Page) {
    // println("In addPage, properties: " + properties)
    if(!_pages.exists(_.get==Some(page))) {
      _pages ::= new WeakReference(page)
      properties.foreach{_ addPage page}
    }
  }
  /**
   * Returns the Elem used to initially incorporate this RElem in the view.
   * If called inside a Lift request, registers the current page with this RElem first.
   * @return an Elem consisting of baseElem plus attributes contributed by events and properties, not to mention the id.
   */
  def render: Elem = {
//    println("Rendering " + getClass + "@" + System.identityHashCode(this))
    if(S.request.isEmpty) println("Warning: Rendering even though there is no current request")
    S.request.foreach{_ =>
      // println("Calling addPage")
      addPage(CurrentPage.is)
    }
    val e = baseElem % new UnprefixedAttribute("id", id, Null)
    val withProps = properties.foldLeft(e){
      case (e, prop) => e % prop.asAttribute
    }
    events.foldLeft[Elem](withProps){
      case (e, evt: DOMEventSource[_]) => e % evt.asAttribute
      case (e, _) => e
    }
  }
  
  def asHtml = render
}
