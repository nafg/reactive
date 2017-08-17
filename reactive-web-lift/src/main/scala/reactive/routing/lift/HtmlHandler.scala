package reactive
package routing
package lift

import scala.xml.{Node, NodeSeq}
import net.liftweb.common.{Box, Full}
import net.liftweb.http.{LiftResponse, NotFoundResponse, Req, S, XhtmlResponse}

/**
 * A request handler that returns html, based on a provided template
 */
trait HtmlResponse extends (Req => LiftResponse) {
  private object OneNode {
    def unapply(ns: NodeSeq): Option[Node] = ns match {
      case n: Node                       => Some(n)
      case ns: NodeSeq if ns.length == 1 => Some(ns(0))
      case _                             => None
    }
  }

  /**
   * The template html, which
   * will be run through the Lift merge phase.
   */
  def template: Box[NodeSeq]

  /**
   * Compute the response.
   * By default, runs [[template]] through the merge phase,
   * and returns the doctype, headers, and cookies from `net.liftweb.http.S`.
   * If `template` or `S.session` is empty, or merging does not return a
   * single `Node`, returns a `NotFoundResponse`.
   */
  def apply(req: Req): LiftResponse = {
    val resp = for {
      t        <- template
      session  <- S.session
      response <- session.processTemplate(Full(t), req, req.path, 200)
    } yield response
    resp openOr NotFoundResponse()
  }
}
