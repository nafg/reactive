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

  implicit class StringPathOps(s: String) {
    type P = PLit[PNil]
    def path: P = PLit(s, PNil)
    def :/:(s: String) = PLit[P](s, path)
    def :/:[A](arg: Arg[A]) = PArg[A, P](arg, path)
    def >>[A](route: P#Route[A]): PathRoute[P, A] = new PathRoute[P, A](path, route)
    def construct = implicitly[CanEncodePath[P]].encode(path, Location(Nil))
  }
  implicit class ArgPathOps[A](arg: Arg[A]) {
    type P = PArg[A, PNil]
    def path: P = PArg(arg, PNil)
    def :/:(s: String) = PLit[P](s, path)
    def :/:[A](arg2: Arg[A]) = PArg[A, P](arg2, path)
    def >>[A](route: P#Route[A]): PathRoute[P, A] = new PathRoute[P, A](path, route)
    def construct = implicitly[CanEncodePath[P]].encode(path, Location(Nil))
  }
  implicit class ParamPathOps[A](param: Param[A]) {
    type P = PParam[A, PNil]
    def path: P = PParam(param, PNil)
    def :&:(s: String) = PLit[P](s, path)
    def :&:[A](arg: Arg[A]) = PArg[A, P](arg, path)
    def :&:[A](p: Param[A]) = PParam[A, P](p, path)
    def :&:[A](p: Params[A]) = PParams[A, P](p, path)
    def >>[A](route: P#Route[A]): PathRoute[P, A] = new PathRoute[P, A](path, route)
    def construct = implicitly[CanEncodePath[P]].encode(path, Location(Nil))
  }
  implicit class ParamsPathOps[A](params: Params[A]) {
    type P = PParams[A, PNil]
    def path: P = PParams(params, PNil)
    def :&:(s: String) = PLit[P](s, path)
    def :&:[A](arg: Arg[A]) = PArg[A, P](arg, path)
    def :&:[A](p: Param[A]) = PParam[A, P](p, path)
    def :&:[A](p: Params[A]) = PParams[A, P](p, path)
    def >>[A](route: P#Route[A]): PathRoute[P, A] = new PathRoute[P, A](path, route)
    def construct = implicitly[CanEncodePath[P]].encode(path, Location(Nil))
  }
}
