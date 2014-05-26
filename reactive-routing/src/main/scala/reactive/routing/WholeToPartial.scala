package reactive
package routing

sealed trait WholeToPartial0[RT <: RouteType, WholeRoute[_]] {
  type PartialRoute[R] = RT#Route[R]
  def apply[R](w: WholeRoute[R]): PartialRoute[R]
}
object WholeToPartial0 {
  type Id[A] = A
  class FunFrom[A, W[_]] {
    type T[R] = A => W[R]
  }
  implicit val nil: WholeToPartial0[RouteType.Const, Id] = new WholeToPartial0[RouteType.Const, Id] {
    def apply[R](w: R) = w
  }
//  implicit val any = new WholeToPartial0[PAny, FunFrom[List[String], Id]#T] {
//    def apply[R](w: List[String] => R) = PartialFunction(w)
//  }
//  implicit def lit[N <: RouteType, W[_]](implicit next: WholeToPartial0[N, W]): WholeToPartial0[PLit[N], W] = new WholeToPartial0[PLit[N], W] {
//    def apply[R](w: W[R]) = next(w)
//  }
  implicit def arg[A, N <: RouteType, W[_]](implicit next: WholeToPartial0[N, W]) = new WholeToPartial0[RouteType.PF[A, N], FunFrom[A, W]#T] {
    def apply[R](w: A => W[R]): PartialRoute[R] = { case a => next(w(a)) }
  }
//  implicit def param[A, N <: RouteType, W[_]](implicit next: WholeToPartial0[N, W]) = new WholeToPartial0[PParam[A, N], FunFrom[Option[A], W]#T] {
//    def apply[R](w: Option[A] => W[R]): PartialFunction[Option[A], N#Route[R]] = { case a => next(w(a)) }
//  }
//  implicit def params[A, N <: RouteType, W[_]](implicit next: WholeToPartial0[N, W]) = new WholeToPartial0[PParams[A, N], FunFrom[List[A], W]#T] {
//    def apply[R](w: List[A] => W[R]): PartialFunction[List[A], N#Route[R]] = { case a => next(w(a)) }
//  }
}
