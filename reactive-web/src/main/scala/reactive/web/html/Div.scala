package reactive
package web
package html


/**
 * Represents a div DOM element
 */
trait Div extends RElem {
  /**
   * The dblclick event
   */
  lazy val dblClick = DOMEventSource.dblClick
  /**
   * The keyUp event
   */
  lazy val keyUp = DOMEventSource.keyUp
  
  def events = List(dblClick, keyUp)
  def properties = Nil
  def baseElem = <div />
}

/**
 * Provides several factories for creating Divs
 */
object Div {
  import scala.xml.{Elem, NodeSeq}
  
  /**
   * Creates a Repeater Div that contains the RElems contained in the SeqSignal.
   * @param content the SeqSignal
   */
  def apply(content: SeqSignal[RElem])(implicit config: CanRenderDomMutationConfig) = new Div with Repeater {
    def renderer = config.domMutationRenderer
    def children = content
  }

  /**
   * Returns a function for Lift binding that renders a Repeater Div whose contents are
   * bound by the function values of the SeqSignal.
   * The function's input is passed to each function in the SeqSignal. 
   * For example:
   * "div" #> Div(intSeqSignal map (_ map (i => "*" #> i)))
   * @param binding the SeqSignal[NodeSeq=>NodeSeq] whose elements are bind functions used to generate each child of the Div.
   * @return a NodeSeq=>NodeSeq that on each invocation renders a new Repeater Div.
   */
  def apply(binding: SeqSignal[NodeSeq=>NodeSeq])(implicit p: Page, config: CanRenderDomMutationConfig): NodeSeq=>NodeSeq =
    bindFunc2seqContentFunc(binding){c =>
      apply(c map (_ map {ns => RElem(nodeSeqToElem(ns))}))(config).render
    }
}
