package net.liftweb.reactive {
package snippet {

import _root_.scala.xml._

import _root_.net.liftweb.util.Helpers
	import Helpers._
import net.liftweb.http._
import net.liftweb.reactive._


class HelloWorld extends ReactiveSnippet {
	val field = new TextField
	val span = Span(
    content = field.value.change map Text
  )
	
	def howdy(in: NodeSeq): NodeSeq =
		currentPage.render ++
		field.render ++
		span.render


}
}
}
