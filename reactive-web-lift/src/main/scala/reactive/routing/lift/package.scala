package reactive
package routing

package object lift {
  import net.liftweb.http.{ LiftResponse, Req }
  import net.liftweb.common._
  def reqToLoc(req: Req): Location =
    Location(
      req.path.wholePath,
      req.params.toList.flatMap{ case (k, vs) => vs.map(k -> _) }
    )

  def siteToDispatch(site: Sitelet[_, Req => LiftResponse]): PartialFunction[Req, () => Box[LiftResponse]] = new PartialFunction[Req, () => Box[LiftResponse]] {
    def isDefinedAt(req: Req) = site.run isDefinedAt reqToLoc(req)
    def apply(req: Req) = () => Full(site.run(reqToLoc(req))(req))
  }
}
