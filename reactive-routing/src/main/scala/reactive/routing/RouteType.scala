package reactive
package routing

/**
 * Represents the structure of a [[Path]] or [[Sitelet]]. See subclasses, [[RConst]] and [[RFunc]].
 * @note There are no actual instances of [[RouteType]]. It is used for its type projections.
 */
sealed trait RouteType {
  import scala.language.higherKinds
  type Route[+R]
  type Func[R]
  type EncodeFunc
}

/**
 * Indicates a [[Path]] that is not parameterized, or
 * a route that does not take any parameters.
 * @note Cannot be instantiated.
 */
final class RConst private extends RouteType {
  type Route[+R] = R
  type Func[R] = R
  type EncodeFunc = Location
}

/**
 * Indicates a [[Path]] that is parameterized, or a route
 * that takes a parameter. Chains to another [[RouteType]].
 * @note Cannot be instantiated.
 * @tparam In the parameter type
 * @tparam N the next `RouteType` to chain to
 */
final class RFunc[In, N <: RouteType] private extends RouteType {
  type Route[+R] = PartialFunction[In, N#Route[R]]
  type Func[R] = In => N#Func[R]
  type EncodeFunc = In => N#EncodeFunc
}

trait CanLiftRouteMapping[R <: RouteType] {
  def apply[A, B](f: A => B): R#Route[A] => R#Route[B]
}
object CanLiftRouteMapping {
  implicit val const: CanLiftRouteMapping[RConst] = new CanLiftRouteMapping[RConst] {
    override def apply[A, B](f: A => B) = f
  }
  implicit def pf[In, N <: RouteType](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[RFunc[In, N]] = new CanLiftRouteMapping[RFunc[In, N]] {
    override def apply[A, B](f: A => B) = _ andThen next(f)
  }
}

trait FnToPF[R <: RouteType] {
  type Partial[A] = R#Route[A]
  type Whole[A] = R#Func[A]
  def apply[A](v: Whole[A]): Partial[A]
}
object FnToPF {
  implicit val const: FnToPF[RConst] = new FnToPF[RConst] {
    override def apply[A](a: A) = a
  }
  implicit def fun[In, N <: RouteType](implicit next: FnToPF[N]): FnToPF[RFunc[In, N]] = new FnToPF[RFunc[In, N]] {
    override def apply[R](a: In => N#Func[R]) = {
      case x => next(a(x))
    }
  }
}
