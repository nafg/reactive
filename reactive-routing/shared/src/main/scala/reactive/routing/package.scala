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
   * An alias for [[RFunc]], allowing longer [[RouteType]]s to be written like
   * `Int >>: String >>: Double >>: RConst`
   */
  type >>:[In, Next <: RouteType] = RFunc[In, Next]

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

  implicit class StringPathOps(s: String) extends Path.PathRouteOpsBase[RConst] with Path.PathComponentOpsBase[RConst] {
    val path = PLit(s, PNil)
  }

  implicit class ArgPathOps[A](arg: Arg[A]) extends Path.PathRouteOpsBase[A >>: RConst] with Path.PathComponentOpsBase[A >>: RConst] {
    val path = PArg(arg, PNil)
  }

  implicit class ParamPathOps[A](param: Param[A]) extends Path.PathRouteOpsBase[Option[A] >>: RConst] with Path.PathParamOpsBase[Option[A] >>: RConst] {
    val path = PParam(param, PNil)
  }

  implicit class ParamsPathOps[A](params: Params[A]) extends Path.PathRouteOpsBase[List[A] >>: RConst] with Path.PathParamOpsBase[List[A] >>: RConst] {
    val path = PParams(params, PNil)
  }
}
