package reactive
package routing

import scala.language.higherKinds

/**
  * Represents the structure of a [[Path]] or [[Sitelet]]. See subclasses, [[RConst]] and [[RFunc]].
 * @note There are no actual instances of [[RouteType]]. It is used for its type projections.
 */
sealed trait RouteType {
  type Route[+R]
  type Func[R]
  type EncodeFunc = Func[Location]
}

/**
 * Indicates a [[Path]] that is not parameterized, or
 * a route that does not take any parameters.
 * @note Cannot be instantiated.
 */
final class RConst private extends RouteType {
  type Route[+R] = R
  type Func[R] = R
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
}

trait AndThen[F[_]] {
  def apply[A, B](cont: A => B): F[A] => F[B]
}
object AndThen {
  type Id[A] = A
  implicit def const: AndThen[Id] = new AndThen[Id] {
    def apply[A, B](cont: A => B) = cont
  }
  implicit def func[In, Next <: RouteType](implicit next: AndThen[Next#Func]): AndThen[RFunc[In, Next]#Func] = new AndThen[RFunc[In, Next]#Func] {
    def apply[A, B](cont: A => B) = _ andThen next(cont)
  }
  implicit def pf[In, Next <: RouteType](implicit next: AndThen[Next#Route]): AndThen[RFunc[In, Next]#Route] = new AndThen[RFunc[In, Next]#Route] {
    def apply[A, B](cont: A => B) = _ andThen next(cont)
  }
}

//trait Unlift[R <: RouteType] {
//  def apply[A](pf: R#Route[Option[A]]): R#Route[A]
//}
//object Unlift {
//  implicit def const: Unlift[RConst] = new Unlift[RConst] {
//    override def apply[A](x: Option[A]): A =
//  }
//}
//trait AndThenPF[F[_], G[_, _]] {
//  def apply[A, B](cont: PartialFunction[A, B]): G[F[A], F[B]]
//}
//object AndThenPF {
//  type Id[A] = A
//  implicit def const: AndThenPF[Id, PartialFunction] = new AndThenPF[Id, PartialFunction] {
//    override def apply[A, B](cont: PartialFunction[A, B]) = cont
//  }
//  implicit def pf[In, Next <: RouteType] = new AndThenPF {
//    override def apply[A, B](cont: PartialFunction[A, B]) =
//  }
//}

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
