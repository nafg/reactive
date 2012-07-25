package reactive
package web
package demo
package snippet


class Signal_flatMap extends PageSnippet {
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
