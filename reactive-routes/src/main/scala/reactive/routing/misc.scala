package reactive
package routing


/**
 * A typeclass for converting values to and from strings
 */
trait Stringable[A] {
  def format: A => String
  def parse: String => Option[A]
}

class Extractor[-A, +B](f: A => Option[B]) {
  def unapply(a: A) = f(a)
}

object LiftRoutes {
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
