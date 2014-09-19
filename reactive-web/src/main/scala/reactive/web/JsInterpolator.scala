package reactive
package web

import net.liftweb.json.JValue
import net.liftweb.json.JArray
import upickle._

object AjaxCall {
  private def ajaxFnJs(id: String, n: Int, pageId: String) = {
    val params = (1 to n) map (x => js"x$x")
    val paramList = StringRenderable(params.map(_.render).mkString(","))
    js"(function($paramList){reactive.queueAjax($id)([$paramList]);reactive.doAjax($pageId);})"
  }
  private def addHandler(id: String)(pf: PartialFunction[List[JValue], Unit])(implicit page: Page) = {
    import page.observing
    page.ajaxEvents ?>> {
      case (`id`, JArray(list)) if(pf.isDefinedAt(list)) =>
        pf(list)
    }
  }
  def apply(f: () => Unit)(implicit page: Page): StringRenderable = {
    val Id = page.nextNumber.toString
    addHandler(Id){ case List() => f() }
    ajaxFnJs(Id, 0, page.id)
  }

  def apply(f: JValue => Unit)(implicit page: Page) = {
    val Id = page.nextNumber.toString
    addHandler(Id){ case List(a) => f(a) }
    ajaxFnJs(Id, 1, page.id)
  }

  def apply(f: (JValue, JValue) => Unit)(implicit page: Page) = {
    val Id = page.nextNumber.toString
    addHandler(Id){ case List(a, b) => f(a, b) }
    ajaxFnJs(Id, 2, page.id)
  }

  def apply(f: (JValue, JValue, JValue) => Unit)(implicit page: Page) = {
    val Id = page.nextNumber.toString
    addHandler(Id){ case List(a, b, c) => f(a, b, c) }
    ajaxFnJs(Id, 3, page.id)
  }
}


class JsInterpolatable(val string: String)

object JsInterpolatable {
  implicit class arity0(f: () => Unit)(implicit page: Page) extends JsInterpolatable(AjaxCall(f).string)

  implicit class arity1(f: JValue => Unit)(implicit page: Page) extends JsInterpolatable(AjaxCall(f).string)

  implicit class arity2(f: (JValue, JValue) => Unit)(implicit page: Page) extends JsInterpolatable(AjaxCall(f).string)

  implicit class arity3(f: (JValue, JValue, JValue) => Unit)(implicit page: Page) extends JsInterpolatable(AjaxCall(f).string)

  implicit class Rendered(r: Renderable) extends JsInterpolatable(r.render)

  implicit class Value[A: Writer](a: A) extends JsInterpolatable(write(a))
}

trait JsInterpolator extends Any {
  def sc: StringContext

  def js(args: JsInterpolatable*): StringRenderable =
    StringRenderable(sc.standardInterpolator(identity, args.map(_.string)))
}
