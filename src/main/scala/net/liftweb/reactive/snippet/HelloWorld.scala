package net.liftweb.reactive {
package snippet {

import _root_.scala.xml._

import _root_.net.liftweb.util.{Helpers, BindPlus}
	import Helpers._
	import BindPlus._
import net.liftweb.http._
import net.liftweb.reactive._
import reactive._


class reactive {
  def render = CurrentPage.is.render
}
class HelloWorld extends ReactiveSnippet {
  def tryOrPrint[T](p: =>T): T = try{p}catch{case e=>e.printStackTrace; throw e}
	val field = tryOrPrint{new TextField}
	tryOrPrint{
	field.dblClick.eventStream.alive.foreach { _ =>
	  println("Double-click LIVE!")
	}
	field.dblClick.eventStream foreach { case DblClick(modifiers) =>
	  field.value.value ()= field.value.value.now + "*"
	}
	}
	val span = Span(
    content = field.value.value.change map Text
  )
  
  val clockSpan = Span(
    content = new Clock_(interval = 1000).change.map(t => Text((t/1000).toString))
  )
	
	def howdy(in: NodeSeq): NodeSeq = {println("howdy");in.bind("re",
    "field" -> field.render,
    "span" -> span.render,
    "clock" -> clockSpan.render
  )
	}
}
}
}
