package reactive.web.demo.snippet

import reactive._


class Signal_flatMap extends Observing {
  val var1 = Var("This is a Var")
  val var2 = Var("This is also a Var")
  
  /*
   * equivalent to:
   * var1.flatMap(v1 =>
   *   var2.map(v2 =>
   *     "var1: " + v1 + "var2: " + v2
   *   )
   * )
   */
  val flatMapped = for{
    v1 <- var1
    v2 <- var2
  } yield "var1: '" + v1 + "'; var2: '" + v2 + "'"
  
  def render = Demos.varInput(var1) ++
    Demos.varInput(var2) ++
    Demos.signalOutput(flatMapped)
}
