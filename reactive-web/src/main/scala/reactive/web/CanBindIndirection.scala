package reactive.web

import scala.xml.NodeSeq

import net.liftweb.util.CanBind
import reactive.{SeqSignal, Signal}


class CanBindIndirection[-A](val f: A => NodeSeq => Seq[NodeSeq])

trait CanBindIndirectionLow {
  implicit def canBindSignal[A](implicit cba: CanBind[A], page: Page, rdm: CanRenderDomMutationConfig): CanBindIndirection[Signal[A]] = new CanBindIndirection[Signal[A]](
    sig => ns => Cell {
      sig.map(a => (ns: NodeSeq) => cba(a)(ns).foldLeft(NodeSeq.Empty)(_ ++ _))
    } apply ns
  )
}

object CanBindIndirection extends CanBindIndirectionLow {
  implicit def canBindPropertyVar[A](implicit page: Page): CanBindIndirection[PropertyVar[A]] = new CanBindIndirection[PropertyVar[A]](
    pv => ns => pv.render(page) apply ns
  )
  implicit def canBindSeqSignal[A](implicit cba: CanBind[A], page: Page, rdm: CanRenderDomMutationConfig): CanBindIndirection[SeqSignal[A]] = new CanBindIndirection[SeqSignal[A]](
    sig => ns => Repeater {
      sig.now.map(a => (ns: NodeSeq) =>
        cba(a)(ns).foldLeft(NodeSeq.Empty)(_ ++ _)
      ).signal
    } apply ns
  )
}
