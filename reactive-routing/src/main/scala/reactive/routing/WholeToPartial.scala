package reactive
package routing

sealed trait WholeToPartial0[P <: Path, WholeRoute[_]] {
  type PartialRoute[R] = P#PartialRoute[R]
  def apply[R](w: WholeRoute[R]): PartialRoute[R]
}
object WholeToPartial0 {
  type Id[A] = A
  class FunFrom[A, W[_]] {
    type T[R] = A => W[R]
  }
  implicit val nil: WholeToPartial0[PNil, Id] = new WholeToPartial0[PNil, Id] {
    def apply[R](w: R) = w
  }
  implicit val any = new WholeToPartial0[PAny, FunFrom[List[String], Id]#T] {
    def apply[R](w: List[String] => R) = PartialFunction(w)
  }
  implicit def lit[N <: Path, W[_]](implicit next: WholeToPartial0[N, W]): WholeToPartial0[PLit[N], W] = new WholeToPartial0[PLit[N], W] {
    def apply[R](w: W[R]) = next(w).asInstanceOf[PartialRoute[R]]
  }
  implicit def arg[A, N <: Path, W[_]](implicit next: WholeToPartial0[N, W]) = new WholeToPartial0[PArg[A, N], FunFrom[A, W]#T] {
    def apply[R](w: A => W[R]): PartialRoute[R] = { case a => next(w(a)) }
  }
  implicit def param[A, N <: Path, W[_]](implicit next: WholeToPartial0[N, W]) = new WholeToPartial0[PParam[A, N], FunFrom[Option[A], W]#T] {
    def apply[R](w: Option[A] => W[R]): PartialFunction[Option[A], N#PartialRoute[R]] = { case a => next(w(a)) }
  }
  implicit def params[A, N <: Path, W[_]](implicit next: WholeToPartial0[N, W]) = new WholeToPartial0[PParams[A, N], FunFrom[List[A], W]#T] {
    def apply[R](w: List[A] => W[R]): PartialFunction[List[A], N#PartialRoute[R]] = { case a => next(w(a)) }
  }
}
