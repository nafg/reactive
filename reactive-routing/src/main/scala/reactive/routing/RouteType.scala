package reactive
package routing

sealed trait RouteType {
  import scala.language.higherKinds
  type Route[+R]
  type Func[R]
  type EncodeFunc
}
object RouteType {
  trait Const extends RouteType {
    type Route[+R] = R
    type Func[R] = R
    type EncodeFunc = Location
  }
  trait PF[In, N <: RouteType] extends RouteType {
    type Route[+R] = PartialFunction[In, N#Route[R]]
    type Func[R] = In => N#Func[R]
    type EncodeFunc = In => N#EncodeFunc
  }
}

trait CanLiftRouteMapping[R <: RouteType] {
  def apply[A, B](f: A => B): R#Route[A] => R#Route[B]
}
object CanLiftRouteMapping {
  implicit val const: CanLiftRouteMapping[RouteType.Const] = new CanLiftRouteMapping[RouteType.Const] {
    override def apply[A, B](f: A => B) = f
  }
  implicit def pf[In, N <: RouteType](implicit next: CanLiftRouteMapping[N]): CanLiftRouteMapping[RouteType.PF[In, N]] = new CanLiftRouteMapping[RouteType.PF[In, N]] {
    override def apply[A, B](f: A => B) = _ andThen next(f)
  }
}

trait FnToPF[R <: RouteType] {
  type Partial[A] = R#Route[A]
  type Whole[A] = R#Func[A]
  def apply[A](v: Whole[A]): Partial[A]
}
object FnToPF {
  implicit val const: FnToPF[RouteType.Const] = new FnToPF[RouteType.Const] {
    override def apply[A](a: A) = a
  }
  implicit def fun[In, N <: RouteType](implicit next: FnToPF[N]): FnToPF[RouteType.PF[In, N]] = new FnToPF[RouteType.PF[In, N]] {
    override def apply[R](a: In => N#Func[R]) = {
      case x => next(a(x))
    }
  }
}
