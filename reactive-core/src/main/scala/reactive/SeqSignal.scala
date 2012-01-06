package reactive

/**
 * This trait provides special behavior implementations for signals of sequences,
 * preserving the transformation relationship of derived signals by propagating
 * deltas (SeqDeltas).
 */
trait SeqSignal[+A] extends Signal[DeltaSeq[A]] {
  /**
   * The EventStream of incremental updates (SeqDeltas) to the underlying Seq.
   */
  lazy val deltas: EventStream[SeqDelta[A, A]] = change.map(_.fromDelta)

}

object SeqSignal {
  def defaultDiffFunc[A]: (Seq[A], Seq[A]) => Seq[SeqDelta[A, A]] =
    { (a: Seq[A], b: Seq[A]) => LCS.lcsdiff[A, A](a, b, _ == _) }

  /**
   * This factory creates a SeqSignal that wraps an ordinary Signal[Seq[_]],
   * with the behavior that whenever the original signal's value changes,
   * a diff is calculated and the new SeqSignal fires deltas representing
   * the change incrementally.
   */
  def apply[A](orig: Signal[Seq[A]],
               diffFunc: (Seq[A], Seq[A]) => Seq[SeqDelta[A, A]] = defaultDiffFunc[A],
               includeInit: Boolean = true): SeqSignal[A] = new SeqSignal[A] with Logger { owner =>
    val underlying: Signal[DeltaSeq[A]] = orig.foldLeft(if (includeInit) Nil else List(orig.now.toList)){
      case (old, xs) =>
        xs.toList :: old.take(1)
    }.map {
      case Nil => DeltaSeq()
      case b :: a => new DeltaSeq[A] {
        val underlying = b
        val fromDelta = Batch(diffFunc(a.headOption getOrElse Nil, b): _*)
        val signal = owner
      }
    }(CanMapSignal.canMapSignal)
    def change = underlying.change
    def now = underlying.now
    override def toString = "SeqSignal("+now+")"
  }

  /**
   * Given a Signal[Seq[A]], return an EventStream that fires the diff represeting every change.
   */
  def diffStream[A](orig: Signal[Seq[A]],
                    diffFunc: (Seq[A], Seq[A]) => Seq[SeqDelta[A, A]] = defaultDiffFunc,
                    includeInit: Boolean = false): EventStream[SeqDelta[A, A]] =
    orig.foldLeft((Seq.empty[A], if (includeInit) Nil else orig.now)){
      case ((_, old), xs) =>
        (old.toList, xs.toList)
    }.map{ case (a, b) => Batch(diffFunc(a, b): _*) }.change

  //TODO optimized shortcut for apply(Val(seq))
}

// TODO reimplement using DeltaSeq.updatedFromParent
//class MappedSeqSignal[T, E](
//  private val parent: Signal[T],
//  f: T => TransformedSeqBase[E]) extends ChangingSeqSignal[E] with Logger {
//  case class NotPropagatingDelta(delta: SeqDelta[T, T]) extends LogEventPredicate
//  case object DoesntHaveSeqSignalParent extends LogEventPredicate
//
//  def now = underlying
//  override def transform = underlying
//  def underlying: TransformedSeqBase[E] = _underlying
//  private var _underlying: TransformedSeqBase[E] = f(parent.now)
//
//  private val parentChangeListener = { x: T =>
//    _underlying = f(x)
//  }
//  parent.change addListener parentChangeListener
//
//  override lazy val deltas = new EventSource[SeqDelta[E, E]] {}
//  private val deltasListener: SeqDelta[T, T] => Unit = { m =>
//    underlying match {
//      case t: TransformedSeq[T]#Transformed[E] =>
//        SeqDelta.single(t.xform(m)) foreach deltas.fire
//      case _ =>
//        trace(NotPropagatingDelta(m))
//    }
//  }
//  parent match {
//    case ss: SeqSignal[T] =>
//      ss.deltas addListener deltasListener
//    case _ =>
//      trace(DoesntHaveSeqSignalParent)
//  }
//
//  override def toString = "MappedSeqSignal("+parent+","+Util.debugString(f)+")"
//}
//
//protected class FlatMappedSeqSignal[T, U](private val parent: Signal[T], f: T => SeqSignal[U]) extends SeqSignalImpl[U] {
//  def now = currentMappedSignal.now
//  override lazy val change = new EventSource[TransformedSeqBase[U]] {}
//  private val changeListener: TransformedSeqBase[U] => Unit = change.fire _
//  private val deltasListener: SeqDelta[U, U] => Unit = transform.deltas.fire _
//  private var currentMappedSignal = f(parent.now)
//  override lazy val transform = new TransformedSeq[U] {
//    def underlying = currentMappedSignal.now
//    override lazy val deltas = new EventSource[SeqDelta[U, U]]
//  }
//  private var lastSeq: Seq[U] = now
//  currentMappedSignal.change addListener changeListener
//  currentMappedSignal.deltas addListener deltasListener
//
//  private val parentChangeListener: T => Unit = { x =>
//    currentMappedSignal.change removeListener changeListener
//    currentMappedSignal.deltas removeListener deltasListener
//    currentMappedSignal = f(x)
//    val n = currentMappedSignal.transform
//    change fire n
//    lastSeq = fireDeltaDiff(lastSeq, n)
//    currentMappedSignal.change addListener changeListener
//    currentMappedSignal.deltas addListener deltasListener
//  }
//  parent.change addListener parentChangeListener
//
//  private def fireDeltaDiff(lastSeq: Seq[U], newSeq: Seq[U]): Seq[U] = {
//    transform.deltas fire Batch(LCS.lcsdiff(lastSeq, newSeq, (_: U) == (_: U)): _*)
//    newSeq
//  }
//
//  override def toString = "FlatMappedSeqSignal("+parent+","+System.identityHashCode(f)+")"
//}


