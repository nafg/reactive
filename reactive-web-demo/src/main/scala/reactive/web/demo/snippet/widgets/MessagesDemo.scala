package reactive
package web
package demo
package snippet.widgets

import scala.xml.{NodeSeq, Text}

import net.liftweb.util.Helpers._
import reactive.web.html.Button
import reactive.web.widgets.Messages

class MessagesDemo extends PageSnippet {
  def render = "button" #> (DomEventSource.click ->> {
    Messages += "This is a string"
    var counter = 1
    Messages += Text("This is html") ++ Button("Click me"){
      def count(): Unit = {
        lazy val m: NodeSeq = Text("Message #"+counter) ++ Button("Next"){
          Messages -= m
          counter += 1
          count()
        }.render
        Messages += m
      }

      count()
    }.render
  })
}
