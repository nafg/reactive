package reactive
package web
package snippet

import _root_.scala.xml._

import _root_.net.liftweb.util.{Helpers, BindPlus}
  import Helpers._
  import BindPlus._
import net.liftweb.http._



class MainPage extends ReactiveSnippet {
  println("new HelloWorld: " + this)
//  Thread.dumpStack()
//  println
  //throw new Exception
  def tryOrPrint[T](p: =>T): T = try{p}catch{case e=>e.printStackTrace; throw e}
  val field = tryOrPrint{TextInput()}
//  field.value updateOn field.keyUp
  field.value.value foreach {v =>
    println("Value changed: " + v)
  }
  tryOrPrint{
    field.dblClick.eventStream.alive.foreach { _ =>
      println("Double-click LIVE!")
    }
    field.dblClick.eventStream foreach { case DblClick(modifiers) =>
      field.value.value ()= field.value.value.now + "*"
    }
  }
  val span = Span(
    content = field.value.value map Text
  )
  val clock = new _root_.reactive.Timer(interval = 5000).change.takeWhile(_ => isPageAlive).hold[Long](0)
  val clockSpan = Span(
    content = clock.map(t => Text((t/1000).toString))
  )
  val items = new BufferSignal[Int] {
    def observing = currentPage
  }
  val spanTemplate = <p>[[<re:span />]]</p>
  def bindSpan(i: Int) = RElem.bindPF(spanTemplate) {
    case <re:span /> => Span(Val(Text(i.toString)))
  }
  val spans = items.map{s: TransformedSeq[Int] =>
    //s.flatMap(i => Seq(Left(new TextSpan(Val(i.toString))): RParentElem#Child, Right(<b>:</b>)))
    s.flatMap(i => bindSpan(i))
  }
  spans.deltas foreach {m => println(this + ".spans: " + m)}
  val div = tryOrPrint(new Div(spans))
  var last = 0
  val page = currentPage
  tryOrPrint{clock.foreach {_ =>
    if(items.now.length == 0 || math.random > .5) {
      println(page.id + ": adding " + last + " to " + items.now)
      items.value += last
      last += 1
    } else {
      println(page.id + ": removing from " + items.now)
      items.value.remove(math.random*items.now.length toInt)
    }
  }}

  
  def render(in: NodeSeq): NodeSeq = {
    in.bind("re",
      "field" -> field,
      "span" -> span,
      "clock" -> clockSpan,
      "div" -> div
    )
  }
}
