package reactive
package routing

/**
 * This typeclass provides the ability to run
 * a path + route and produce a result.
 * Used [[Sitelet#mapPath]] and
 * to enrich [[Path]] with `>>` via [[Path.PathRouteOps]]
 */
//trait CanRunPath1[P <: Path] {
//  type PartialRoute[R] = P#PartialRoute[R]
//  type WholeRoute[R] = P#WholeRoute[R]
//  def run[R](path: P, route: P#PartialRoute[R]): PartialFunction[Location, R]
//  def wholeToPartial[R](whole: WholeRoute[R]): PartialRoute[R]
//}
//object CanRunPath1 {
//}
