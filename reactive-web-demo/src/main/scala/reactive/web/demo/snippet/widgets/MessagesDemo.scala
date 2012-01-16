package reactive.web.demo.snippet.widgets

import scala.xml.Text
import scala.xml.NodeSeq

import net.liftweb.util.Helpers._

import reactive.Observing
import reactive.web.DomEventSource
import reactive.web.widgets.Messages
import reactive.web.html.Button

class MessagesDemo extends Observing {
  def render = "button" #> (DomEventSource.click ->> {
    Messages += "This is a string"
    var counter = 1
    Messages += Text("This is html") ++ Button("Click me"){
      def count {
        lazy val m: NodeSeq = Text("Message #"+counter) ++ Button("Next"){
          Messages -= m
          counter += 1
          count
        }.render
        Messages += m
      }
      count
    }.render
  })
}
