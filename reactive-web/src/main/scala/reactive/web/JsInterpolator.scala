package reactive
package web

import net.liftweb.json._
import upickle._

object LiftJsonToUpickle {
  def apply(in: JValue): Js.Value = in match {
    case JString(s)       => Js.Str(s)
    case JBool(true)      => Js.True
    case JBool(false)     => Js.False
    case JInt(i)          => Js.Str(i.toString)
    case JDouble(d)       => Js.Num(d)
    case JNull | JNothing => Js.Null
    case JArray(xs)       => Js.Arr(xs map apply: _*)
    case JObject(xs)      => Js.Obj(xs map (f => (f.name, apply(f.value))): _*)
    case JField(_, _)     => sys.error("JField is not a value")
  }
  def unapply(in: JValue) = Some(apply(in))
  implicit def writer: Writer[JValue] = Writer(apply)
}
object UpickleToLiftJson {
  def apply(in: Js.Value): JValue = in match {
    case Js.Str(s)        => JString(s)
    case Js.True          => JBool(true)
    case Js.False         => JBool(false)
    case Js.Num(d)        => if(d == d.toInt) JInt(d.toInt) else JDouble(d)
    case Js.Null          => JNull
    case Js.Arr(xs @ _*)  => JArray(xs.map(apply).toList)
    case Js.Obj(xs @ _*)  => JObject(xs.map(t => JField(t._1, apply(t._2))).toList)
  }
  implicit def reader: Reader[JValue] = Reader{ case x => apply(x) }
}

object AjaxCall {
  private def ajaxFnJs(id: String, n: Int, pageId: String) = {
    val params = (1 to n) map (n => js"x$n")
    val paramList = StringRenderable(params.map(_.render).mkString(","))
    js"(function($paramList){reactive.queueAjax($id)([$paramList]);reactive.doAjax($pageId);})"
  }
  private def addHandler(id: String)(pf: PartialFunction[Seq[Js.Value], Unit])(implicit page: Page) = {
    import page.observing
    page.ajaxEvents ?>> {
      case (`id`, LiftJsonToUpickle(Js.Arr(xs @ _*))) if(pf.isDefinedAt(xs)) =>
        pf(xs)
    }
  }
  def apply(f: () => Unit)(implicit page: Page): StringRenderable = {
    val id = page.nextNumber.toString
    addHandler(id){ case Seq() => f() }
    ajaxFnJs(id, 0, page.id)
  }

  def apply[P1: Reader](f: P1 => Unit)(implicit page: Page) = {
    val id = page.nextNumber.toString
    addHandler(id){ case Seq(a) => f(readJs[P1](a)) }
    ajaxFnJs(id, 1, page.id)
  }

  def apply[P1: Reader, P2: Reader](f: (P1, P2) => Unit)(implicit page: Page) = {
    val id = page.nextNumber.toString
    addHandler(id){ case Seq(a, b) => f(readJs[P1](a), readJs[P2](b)) }
    ajaxFnJs(id, 2, page.id)
  }

  def apply[P1: Reader, P2: Reader, P3: Reader](f: (P1, P2, P3) => Unit)(implicit page: Page) = {
    val id = page.nextNumber.toString
    addHandler(id){ case Seq(a, b, c) => f(readJs[P1](a), readJs[P2](b), readJs[P3](c)) }
    ajaxFnJs(id, 3, page.id)
  }
}


trait AsJs[A] {
  def render(a: A): String
}
object AsJs {
  def apply[A](f: A => String): AsJs[A] = new AsJs[A] { def render(a: A) = f(a) }

  implicit def arity0(implicit page: Page): AsJs[() => Unit] = AsJs(f => AjaxCall(f).string)
  implicit def arity1[P1: Reader](implicit page: Page): AsJs[P1 => Unit] = AsJs(f => AjaxCall(f).string)
  implicit def arity2[P1: Reader, P2: Reader](implicit page: Page): AsJs[(P1, P2) => Unit] = AsJs(f => AjaxCall(f).string)
  implicit def arity3[P1: Reader, P2: Reader, P3: Reader](implicit page: Page): AsJs[(P1, P2, P3) => Unit] = AsJs(f => AjaxCall(f).string)

  implicit def renderable[R <: Renderable]: AsJs[R] = AsJs(_.render)

  implicit def value[A: Writer]: AsJs[A] = AsJs(a => write(a))
}

final class JsInterpolatable(string: String) {
  def toRenderable = StringRenderable(string)
}
object JsInterpolatable {
  implicit def interpolatable[A: AsJs](a: A): JsInterpolatable = new JsInterpolatable(implicitly[AsJs[A]].render(a))
}
class JsInterpolator(val sc: StringContext) extends AnyVal {
  def js(args: JsInterpolatable*): StringRenderable =
    StringRenderable(sc.standardInterpolator(identity, args.map(_.toRenderable.render)))
}
