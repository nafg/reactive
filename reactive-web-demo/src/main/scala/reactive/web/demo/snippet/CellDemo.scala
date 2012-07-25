package reactive
package web
package demo
package snippet


import net.liftweb.util.Helpers._

import xml.NodeSeq



class CellDemo extends Observing {
  implicit val page = Page.currentPage

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
