package reactive
package web

import scala.language.implicitConversions

import net.liftweb.json._

import io.circe.syntax.EncoderOps
import io.circe.{CursorOp, Decoder, Encoder, Json}


object LiftJsonToCirce {
  def apply(in: JValue): Json = in match {
    case JString(s)       => Json.fromString(s)
    case JBool(b)         => Json.fromBoolean(b)
    case JInt(i)          => Json.fromBigInt(i)
    case JDouble(d)       => Json.fromBigDecimal(d)
    case JNull | JNothing => Json.Null
    case JArray(xs)       => Json.arr(xs map apply: _*)
    case JObject(xs)      => Json.obj(xs map (f => (f.name, apply(f.value))): _*)
  }
  def unapply(in: JValue) = Some(apply(in))
}

object CirceArray {
  def unapply(json: Json): Option[Seq[Json]] = json.asArray
}

object CirceToLiftJson {
  def apply(in: Json): JValue = in.fold(
    jsonNull = JNull,
    jsonBoolean = JBool,
    jsonNumber = n => n.toBigInt.map(JInt).getOrElse(JDouble(n.toDouble)),
    jsonString = JString,
    jsonArray = xs => JArray(xs.map(apply).toList),
    jsonObject = xs => JObject(xs.toList.map(t => JField(t._1, apply(t._2))))
  )
}

object AjaxCall {
  private def ajaxFnJs(id: String, n: Int, pageId: String) = {
    val params = (1 to n) map (n => js"x$n")
    val paramList = StringRenderable(params.map(_.render).mkString(","))
    js"(function($paramList){reactive.queueAjax($id)([$paramList]);reactive.doAjax($pageId);})"
  }
  private def addHandler(id: String)(pf: PartialFunction[Seq[Json], Unit])(implicit page: Page) = {
    import page.observing
    page.ajaxEvents ?>> {
      case (`id`, LiftJsonToCirce(CirceArray(xs))) if pf isDefinedAt xs =>
        pf(xs)
    }
  }
  private def decode[A: Decoder](json: Json): A =
    Decoder[A].decodeJson(json) match {
      case Right(x)      => x
      case Left(failure) =>
        throw failure.withMessage("Decoding failure at " + CursorOp.opsToPath(failure.history) + ": " + failure.message)
    }

  def apply(f: () => Unit)(implicit page: Page): StringRenderable = {
    val id = page.nextNumber.toString
    addHandler(id) { case Seq() => f() }
    ajaxFnJs(id, 0, page.id)
  }

  def apply[P1: Decoder](f: P1 => Unit)(implicit page: Page) = {
    val id = page.nextNumber.toString
    addHandler(id) { case Seq(a) => f(decode[P1](a)) }
    ajaxFnJs(id, 1, page.id)
  }

  def apply[P1: Decoder, P2: Decoder](f: (P1, P2) => Unit)(implicit page: Page) = {
    val id = page.nextNumber.toString
    addHandler(id) { case Seq(a, b) => f(decode[P1](a), decode[P2](b)) }
    ajaxFnJs(id, 2, page.id)
  }

  def apply[P1: Decoder, P2: Decoder, P3: Decoder](f: (P1, P2, P3) => Unit)(implicit page: Page) = {
    val id = page.nextNumber.toString
    addHandler(id) { case Seq(a, b, c) => f(decode[P1](a), decode[P2](b), decode[P3](c)) }
    ajaxFnJs(id, 3, page.id)
  }
}


trait AsJs[A] {
  def render(a: A): String
}
object AsJs {
  def apply[A](f: A => String): AsJs[A] = new AsJs[A] {
    def render(a: A) = f(a)
  }

  implicit def arity0(implicit page: Page): AsJs[() => Unit] = AsJs(f => AjaxCall(f).string)
  implicit def arity1[P1: Decoder](implicit page: Page): AsJs[P1 => Unit] = AsJs(f => AjaxCall(f).string)
  implicit def arity2[P1: Decoder, P2: Decoder](implicit page: Page): AsJs[(P1, P2) => Unit] = AsJs(f => AjaxCall(f).string)
  implicit def arity3[P1: Decoder, P2: Decoder, P3: Decoder](implicit page: Page): AsJs[(P1, P2, P3) => Unit] = AsJs(f => AjaxCall(f).string)

  implicit def renderable[R <: Renderable]: AsJs[R] = AsJs(_.render)

  implicit def value[A: Encoder]: AsJs[A] = AsJs(_.asJson.noSpaces)
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
