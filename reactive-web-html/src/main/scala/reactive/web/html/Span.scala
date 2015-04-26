package reactive
package web
package html


import scala.xml.NodeSeq


/**
 * Represents a span element in the DOM
 */
trait Span extends RElem {
  def baseElem = <span/>
  def events: Seq[DomEventSource[_ <: DomEvent]] = Nil
  def properties: Seq[PropertyVar[_]] = Nil
  
  override def toString = "Span(" + baseElem + ")"
}

/**
 * Provides several factories for creating Spans
 */
object Span {
  /**
   * Create a Span Cell whose contents are represented by a given Signal.
   * @param content the Signal[NodeSeq] that represents the contents of the Span. Defaults to being empty.
   * @return a Span Cell
   */
  def apply(content: Signal[NodeSeq] = Val(NodeSeq.Empty))(implicit config: CanRenderDomMutationConfig) = {
    def tmp = content
    new Span with Cell {
      def renderer = config.domMutationRenderer
      val content = tmp
    }
  }
  /**
   * Returns a function for Lift binding that renders a Span Cell whose contents are
   * bound by the function value of the Signal.
   * The function's input is passed to the value of the Signal. 
   * For example:
   * "span" #> Span(intSignal map (i => "*" #> i))
   * @param binding the Signal[NodeSeq=>NodeSeq] that represents the bind function used to generate the contents of the Span.
   * @return a NodeSeq=>NodeSeq that on each invocation renders a new Span Cell
   */
  def apply(binding: Signal[NodeSeq=>NodeSeq])(implicit p: Page, config: CanRenderDomMutationConfig): NodeSeq=>NodeSeq =
    bindFunc2contentFunc(binding){c =>
      apply(c)(config).render
    }
}

