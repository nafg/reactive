package reactive.web.snippet


import net.liftweb.util.Helpers._

import reactive._
  import web._
import xml.NodeSeq

class CellDemo extends Observing {
// Display the number of elapsed seconds
  val signal: Signal[String] =
    new Timer(1000, until = (_ > 1000*600)).hold(0L).map(n => (n/1000).toString)
  
  def render =
    "#cell" #> Cell {
      signal map {s =>
        "#time" #> s
      }
    }
}
