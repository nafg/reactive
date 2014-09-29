package reactive
package web

import scala.xml.NodeSeq
import net.liftweb.json.JsonAST.JValue

trait TransportType {
  private[web] val ajaxEvents = new EventSource[(String, JValue)] {
    override def debugName = TransportType.this.toString + ".ajaxEvents"
  }

  private[web] val transports = new AtomicRef(List.empty[Transport])

  /**
   * Adds a [[Transport]]
   */
  def linkTransport(t: Transport) =
    transports.transform(t +: _)

  /**
   * Removes a [[Transport]]
   */
  def unlinkTransport(t: Transport) =
    transports.transform { _.filter(t ne _) }

  /**
   * Adds a [[Transport]], runs a block of code, and then removes the transport.
   * @note The [[Transport]] will be visible to other threads.
   */
  def withTransport[A](t: Transport)(f: =>A): A = {
    linkTransport(t)
    try f
    finally unlinkTransport(t)
  }

  /**
   * The returned `NodeSeq` will be added to the document body.
   */
  def render: NodeSeq = NodeSeq.Empty
}
