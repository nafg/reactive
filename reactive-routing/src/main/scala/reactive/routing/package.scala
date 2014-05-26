package reactive

package object routing {
  /**
   * The empty [[Path]].
   */
  val PNil: PNil = PNil0

  /**
   * The `PAny` instance
   */
  val ** : PAny = PAny0

  /**
   * Declare a path component that is converted to and from a value
   * @example {{{ "main" :/: arg[Int]   // e.g. /main/10 }}}
   */
  def arg[A](implicit stringable: Stringable[A]) = new Arg[A](stringable)
  /**
   * Declare a url query parameter that is converted to and from a value
   * @param key the query parameter key
   * @example {{{ "main" :/: param[Int]("item")   // e.g. /main?item=10 }}}
   */
  def param[A](key: String)(implicit stringable: Stringable[A]) = new Param[A](key, stringable)
  /**
   * Declare a repeatable url query parameter that is converted to and from a value
   * @param key the query parameter key
   * @example {{{ "main" :/: param[Int]("options")   // e.g. /main/options=10&options=20 }}}
   */
  def params[A](key: String)(implicit stringable: Stringable[A]) = new Params[A](key, stringable)

  implicit class StringPathOps(s: String) extends Path.PathComponentOpsBase[RouteType.Const] with Path.PathRouteOpsBase[RouteType.Const] {
    def path = PLit(s, PNil)
  }
  implicit class ArgPathOps[A](arg: Arg[A]) extends Path.PathComponentOpsBase[RouteType.PF[A, RouteType.Const]] with Path.PathRouteOpsBase[RouteType.PF[A, RouteType.Const]] {
    def path = PArg(arg, PNil)
  }
  implicit class ParamPathOps[A](param: Param[A]) extends Path.PathParamOpsBase[RouteType.PF[Option[A], RouteType.Const]] with Path.PathRouteOpsBase[RouteType.PF[Option[A], RouteType.Const]] {
    def path = PParam(param, PNil)
  }
  implicit class ParamsPathOps[A](params: Params[A]) extends Path.PathParamOpsBase[RouteType.PF[List[A], RouteType.Const]] with Path.PathRouteOpsBase[RouteType.PF[List[A], RouteType.Const]] {
    def path = PParams(params, PNil)
  }
}
