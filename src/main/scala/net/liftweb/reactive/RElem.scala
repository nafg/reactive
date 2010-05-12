package net.liftweb.reactive


import net.liftweb.http._
	import js._
		import JsCmds._
		import JE._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.actor._
import scala.xml._

import reactive._

trait Renderable {
  def render: NodeSeq
}
object Renderable {
  implicit def toNodeSeq(r: Renderable) = r.render
}

object Span {
  def apply(
    content: EventStream[NodeSeq] = new EventStream[NodeSeq] {}
  )(implicit page: Page) = {
    val contentES = content
    new Span {
      for(change <- contentES) content ()= change
    }
  }
}
class Span(
	contentES: EventStream[NodeSeq] = new EventStream[NodeSeq] {}
)(
	implicit val page: Page
) extends Renderable {
	val content: Var[NodeSeq] = Var(NodeSeq.Empty)
	lazy val id = randomString(20)
	for(change <- contentES) {
	  content ()= change
	}
	page.observe(content) { content =>
	  println("Span content updated")
		Reactions.queueCmd(SetHtml(id, content))
		Reactions.flushQueue
		true
	}
	def render = <span id={id}>{content.now}</span>
	
}
class TextField(
	valueES: EventStream[String] = new EventStream[String] {}
)(implicit val page: Page) {
	def func: List[String]=>Unit = {
		case Nil =>
		case s :: _ => value ()= s
	}
	val value = new Var("")
	page.on(valueES) {v =>
		value ()= v		
		true
	}
	// TODO: onkeyup should set 'value' via ajax
	// TODO: setting value should set input's value via comet
	def render = {
		S.fmapFunc(S.contextFuncBuilder(func)) {funcName =>
			val paramjs = JsRaw("'"+funcName+"=' + encodeURIComponent(this.value)")
			<input type="text"
				value={value.now}
				onkeyup={SHtml.makeAjaxCall(paramjs)}
			/>
    }
  }
}
