package reactive.web.snippet

import reactive._


class Signal_map extends Observing {
  val myVar = Var("This is a Var")
  
  val mapped = myVar.map(s => s.reverse)
  
  def render = Demos.varInput(myVar) ++ Demos.signalOutput(mapped)
}
