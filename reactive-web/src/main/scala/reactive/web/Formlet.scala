package reactive
package web

import javascript.{ JsExp, JsTypes, =|> }

import scala.xml.{ NodeSeq, Text }
import net.liftweb.util.{ CssSel, PassThru }
import net.liftweb.util.Helpers.strToCssBindPromoter

case class SelectCss(selector: String, func: NodeSeq => NodeSeq) {
  def toCssSel = selector #> func
}

trait FormletRenderer[-R, +Out, +Cell] {
  def apply(f: List[R]): Out
  def cell(a: List[R], b: Signal[List[R]]): List[Cell]
}
trait FormletRendererLow {
  implicit object nsFunc extends FormletRenderer[NodeSeq => NodeSeq, NodeSeq => NodeSeq, NodeSeq => NodeSeq] {
    def apply(l: List[NodeSeq => NodeSeq]) = l.foldLeft(identity[NodeSeq] _)(_ andThen _)
    def cell(a: List[NodeSeq => NodeSeq], b: Signal[List[NodeSeq => NodeSeq]]) =
      List(apply(a), web.Cell(b map apply))
  }
}
object FormletRenderer extends FormletRendererLow {
  implicit object cssSel extends FormletRenderer[SelectCss, CssSel, NodeSeq => NodeSeq] {
    def apply(l: List[SelectCss]): CssSel = l.foldLeft("thisbetternotexist" #> PassThru)(_ & _.toCssSel)
    def cell(a: List[SelectCss], signal: Signal[List[SelectCss]]): List[NodeSeq => NodeSeq] =
      apply(a) ::
        signal.map { xs =>
          xs.map {
            css =>
              css.copy(func = web.Cell(css.func))
          }
        }
  }
}

trait Formlet[+A, +R, +E] {
  def signal: Signal[A]
  def rendering: List[R]
  def events: EventStream[E] = EventStream.empty[E]

  def <*>[B, C, R2 >: R, E2 >: E](that: Formlet[B, R2, E2])(implicit ev: A <:< (B => C)): Formlet[C, R2, E2] =
    new Formlet[C, R2, E2] {
      val signal = Formlet.this.signal flatMap { f => that.signal map f }
      val rendering = Formlet.this.rendering ++ that.rendering
      override val events = Formlet.this.events | that.events
    }

  def map[B](f: A => B): Formlet[B, R, E] = new Formlet[B, R, E] {
    val signal = Formlet.this.signal match {
      case s: SeqSignal[_] => f(s.now).asInstanceOf[DeltaSeq[_]].signal.asInstanceOf[Signal[B]]
      case s               => s map f
    }
    val rendering = Formlet.this.rendering
    override val events = Formlet.this.events
  }

  def *>[B, R2 >: R, E2 >: E](that: Formlet[B, R2, E2]): Formlet[B, R2, E2] = map((_: A) => (b: B) => b) <*> that
  def <*[B, R2 >: R, E2 >: E](that: Formlet[B, R2, E2]): Formlet[A, R2, E2] = map((a: A) => (_: B) => a) <*> that

  def render[Out](implicit renderer: FormletRenderer[R, Out]): Out = renderer(rendering)

  //TODO other types than NS=>NS
  def flatMap[B, R2 >: R, E2 >: E, R3](f: A => Formlet[B, R2, E2])(implicit page: Page, renderer: FormletRenderer[R2, _, R3]): Formlet[B, R3, E2] = {
    val thatSig: Signal[Formlet[B, R2, E2]] = signal map f
    new Formlet[B, NodeSeq => NodeSeq, E2] {
      val signal: Signal[B] = thatSig flatMap (_.signal)
      val rendering = renderer.cell(this.rendering, thatSig map (_.rendering))
      override def events = thatSig flatMap (_.events)
    }
  }

  def flatMap[B, R2 >: R, E2 >: E](f: A => SeqSignal[Formlet[B, R2, E2]])(implicit page: Page, renderer: FormletRenderer[R2, NodeSeq => NodeSeq], d: DummyImplicit = null): Formlet[SeqSignal[B], NodeSeq => NodeSeq, E2] = {
    new Formlet[SeqSignal[B], NodeSeq => NodeSeq, Nothing] {
      val signal: Signal[SeqSignal[B]] = Val(
        SeqSignal(
          Formlet.this.signal flatMap { a =>
            SeqSignal(f(a).now.map(_.signal).signal.sequence[B]) //TODO sequence should return a SeqSignal
          }
        )
      )
      val rendering = List(
        Repeater {
          SeqSignal {
            Formlet.this.signal flatMap { a =>
              f(a).now.map{ that => renderer(Formlet.this.rendering ++ that.rendering) }.signal
            }
          }
        }
      )
    }
  }

  //TODO other than NS=>NS (CanBind)
  def #:(s: String)(implicit ev: R <:< (NodeSeq => NodeSeq)) = new Formlet[A, CssSel, E] {
    def signal = Formlet.this.signal
    def rendering = List(s #> Formlet.this.rendering.map{ r =>
      val f = ev(r);
      { ns: NodeSeq =>
        val ret = f(ns)
        println("#:")
        println("  in: "+ns)
        println("  out: "+ret)
        ret
      }
    })
    override def events = Formlet.this.events
  }
}

object Formlet {
  def pure[A](a: A) = new Formlet[A, Nothing, Nothing] {
    def signal = Val(a)
    def rendering = Nil
  }
  def lift[A, B, R, E](f: A => B)(a: Formlet[A, R, E]): Formlet[B, R, E] = a map f
  def lift2[A, B, C, R, E](f: A => B => C)(a: Formlet[A, R, E])(b: Formlet[B, R, E]): Formlet[C, R, E] = (a map f) <*> b
  def apply[A, R](signal0: Signal[A], rendering0: List[R] = Nil) = new Formlet[A, R, Nothing] {
    def signal = signal0
    override def rendering = rendering0
  }
}

/**
 * Intermediate stage in creating a formlet, when it has only one rendering component
 */
class FormletBuilder[+A, +R, +E](signal: Signal[A], rendering: R, events: EventStream[E]) { builder =>
  def toFormlet = new Formlet[A, R, E] {
    val signal = builder.signal
    val rendering = List(builder.rendering)
    override val events = builder.events
  }
}
class NSFuncFormletBuilder[+A, +E](signal: Signal[A], rendering: NodeSeq => NodeSeq, events: EventStream[E]) extends FormletBuilder[A, NodeSeq => NodeSeq, E](signal, rendering, events) {
  def on[E1 <: DomEvent](f: JsExp[JsTypes.JsObj =|> JsTypes.JsVoid])(implicit m: Manifest[E1], ee: EventEncoder[E1], o: Observing) = {
    val des = web.on[E1](f)(m, ee, o)
    new NSFuncFormletBuilder[A, E](signal, rendering andThen des, events)
  }
  def onServer[E1 >: E <: DomEvent](f: E1 => Unit)(implicit m: Manifest[E1], ee: EventEncoder[E1], o: Observing) = {
    val des = web.onServer[E1](f)(m, ee, o)
    new NSFuncFormletBuilder[A, E1](signal, rendering andThen des, events | des.eventStream)
  }
  def byEvent[E1 <: DomEvent, B >: A](f: E1 => A => B)(implicit m: Manifest[E1], ee: EventEncoder[E1]) = {
    val des = new DomEventSource[E1]()(m, ee)
    val es: EventStream[A => B] = des.eventStream.map(f)
    val signal2 = signal.flatMap(a => es.map(_(a))) hold signal.now
    new NSFuncFormletBuilder[B, E](signal2, rendering andThen des, events)
  }
}
object FormletBuilder {
  implicit def toFormlet[A, R, E](b: FormletBuilder[A, R, E]): Formlet[A, R, E] = b.toFormlet
}

object Formlets {
  def value(initial: String)(implicit observing: Observing): NSFuncFormletBuilder[String, Nothing] = {
    val pv = PropertyVar("value")(initial) withEvents DomEventSource.change
    new NSFuncFormletBuilder(
      pv,
      { ns: NodeSeq =>
        println("Formlets.value")
        println("  in: "+ns)
        val ret = pv apply ns
        println("  out: "+ret)
        ret
      },
      EventStream.empty
    )
  }

  def on[E1 <: DomEvent](f: JsExp[JsTypes.JsObj =|> JsTypes.JsVoid])(implicit m: Manifest[E1], ee: EventEncoder[E1], o: Observing) = {
    val des = web.on[E1](f)(m, ee, o)
    new NSFuncFormletBuilder[Unit, Nothing](Val(()), des, EventStream.empty)
  }
  def onServer[E1 <: DomEvent](f: E1 => Unit)(implicit m: Manifest[E1], ee: EventEncoder[E1], o: Observing) = {
    val des = web.onServer[E1](f)(m, ee, o)
    new NSFuncFormletBuilder[Unit, E1](Val(()), des, des.eventStream)
  }
  def byEvent[E1 <: DomEvent, A](f: EventStream[E1] => Signal[A])(implicit m: Manifest[E1], ee: EventEncoder[E1]) = {
    val des = new DomEventSource[E1]()(m, ee)
    val signal = f(des.eventStream)
    new NSFuncFormletBuilder[A, Nothing](signal, des, EventStream.empty)
  }

  def select[A](choices: Seq[A], renderer: A => String = (_: A).toString)(initial: A)(implicit observing: Observing): NSFuncFormletBuilder[A, Nothing] = {
    val s = html.Select(Val(choices), renderer)
    s.selectedItem () = Some(initial)
    new NSFuncFormletBuilder(s.selectedItem map (_ getOrElse initial), s, EventStream.empty)
  }

  def checkbox(initial: Boolean)(implicit observing: Observing): NSFuncFormletBuilder[Boolean, Nothing] = {
    val v = Var(initial)
    val c = html.CheckboxInput(v)
    new NSFuncFormletBuilder(v, c, EventStream.empty)
  }

  def text(text: String): Formlet[Unit, NodeSeq => NodeSeq, Nothing] =
    Formlet(Val(()), List(_ => Text(text)))

  def seq[A, R, R2, E](fs: Formlet[A, R, E]*)(implicit renderer: FormletRenderer[R, R2]): Formlet[Seq[A], R2, E] =
    new Formlet[Seq[A], R2, E] {
      val signal = Val(fs.map(_.signal)).sequence
      val rendering = fs.toList.map(_.render)
      override val events = fs.map(_.events).foldLeft(EventStream.empty[E])(_ | _)
    }

  def repeater[A, R, E](s: SeqSignal[Formlet[A, R, E]])(implicit renderer: FormletRenderer[R, NodeSeq => NodeSeq]) = new Formlet[Seq[A], NodeSeq => NodeSeq, E] {
    val signal = s.now.map(_.signal).signal.sequence
    val rendering = List(
      Repeater {
        s.now.map(f => renderer(f.rendering)).signal
      }
    )
  }
}
