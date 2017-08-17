package reactive
package routing

import net.liftweb.http.{ LiftResponse, Req }
import net.liftweb.common._

package object lift {
  /**
   * Converts a `net.liftweb.http.Req` to a [[Location]]
   */
  def reqToLoc(req: Req): Location =
    Location(
      req.path.wholePath,
      req.params.toList.flatMap{ case (k, vs) => vs.map(k -> _) }
    )

  /**
   * Converts a [[Sitelet]] of `Req => LiftResponse` to the format expected by `LiftRules.dispatch`
   */
  def siteToDispatch(site: Sitelet[_, Req => LiftResponse]): PartialFunction[Req, () => Box[LiftResponse]] = new PartialFunction[Req, () => Box[LiftResponse]] {
    def isDefinedAt(req: Req) = site.run isDefinedAt reqToLoc(req)
    def apply(req: Req) = () => Full(site.run(reqToLoc(req))(req))
  }


  /**
   * Forward a request to different handlers depending on whether the request passes a test
   * @param condition a function that tests the `Req`
   * @param ifTrue the response to return if `condition` returns `true`
   * @param ifFalse the response to return if `condition` returns `false`
   * @example {{{
   *    val loginFirst = guard(testNotLoggedIn, loginRedirectHandler) _
   * }}}
   */
  def guard(condition: Req => Boolean, ifTrue: Req => LiftResponse)(ifFalse: Req => LiftResponse) = { req: Req =>
    if (condition(req)) ifTrue(req) else ifFalse(req)
  }
}
