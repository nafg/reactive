package reactive


/**
 * This trait provides special behavior implementations for signals of sequences,
 * preserving the transformation relationship of derived signals by propagating
 * deltas (SeqDeltas).
 */
//TODO covariance
trait SeqSignal[T] extends SimpleSignal[TransformedSeq[T]] {
  //private def wrapMapping[U](f: Seq[T]=>Seq[U]): Seq[T]=>Seq[U] = {
  //  _ => f(transform)
  //}
  private lazy val underlying: TransformedSeq[T] = new TransformedSeq[T] {
    def underlying = SeqSignal.this.now
  }
  /**
   * Returns the TransformedSeq, the actual delta-propagating Seq.
   */
  def transform: TransformedSeq[T] = underlying

  override lazy val change: EventStream[TransformedSeq[T]] = new EventSource[TransformedSeq[T]] {}

  /**
   * The EventStream of incremental updates (SeqDeltas) to the underlying Seq.
   */
  def deltas = transform.deltas

  //TODO override regular map and check for type at runtime.
  //  def map[U](f: TransformedSeq[T] => TransformedSeq[U]): SeqSignal[U] =
  //    new MappedSeqSignal[U](f)

}

object SeqSignal {
  /**
   * This factory creates a SeqSignal that wraps an ordinary Signal[Seq[_]],
   * with the behavior that whenever the original signal's value changes,
   * a diff is calculated and the new SeqSignal fires deltas representing
   * the change incrementally.
   */
  //TODO this should be built on a public method that just creates an
  //EventStream of the diffs
  def apply[T](orig: Signal[Seq[T]]): SeqSignal[T] =
    new SeqSignal[T] {
      def now = transform
      //TODO cache?
      override lazy val transform = new TransformedSeq[T] {
        def underlying = orig.now
      }
      override lazy val change = orig.change map {s =>
        new TransformedSeq[T] { def underlying = s }
      }

      change addListener changeListener
      private var _prev: List[T] = now.toList
      private lazy val changeListener = { _cur: Seq[T] =>
        val c = _cur.toList
        val diff = LCS.lcsdiff(_prev, c, (_: T) == (_: T))
        transform.deltas.fire(Batch(diff: _*))
        _prev = c
      }
      
      override def toString = "SeqSignal("+now+")"
    }
}

class MappedSeqSignal[T, E](
  private val parent: Signal[T],
  f: T => TransformedSeq[E]
) extends ChangingSeqSignal[E] {
  def now = underlying
  override def transform = underlying
  def underlying: TransformedSeq[E] = _underlying
  private var _underlying: TransformedSeq[E] = f(parent.now)

  private val parentChangeListener = {x: T =>
    _underlying = f(x)
  }
  parent.change addListener parentChangeListener

  override lazy val deltas = new EventSource[SeqDelta[E, E]] {}
  private val deltasListener: SeqDelta[T,T]=>Unit = { m =>
    underlying match {
      case t: TransformedSeq[T]#Transformed[E] =>
        SeqDelta.single(t.xform(m)) foreach deltas.fire
      case _ =>
        println("not propagating delta "+m+": underlying is not a TransformedSeq#Transformed")
    }
  }
  parent match {
    case ss: SeqSignal[T] =>
      println(toString+": parent is a SeqSignal")
      ss.deltas addListener deltasListener
    case _ =>
      println(toString+": parent is not a SeqSignal")
  }

  override def toString = "MappedSeqSignal("+parent+","+f.getClass+"@"+Integer.toHexString(System.identityHashCode(f))+")"
}

protected class FlatMappedSeqSignal[T,U](private val parent: Signal[T], f: T=>SeqSignal[U]) extends SeqSignal[U] {
  def now = currentMappedSignal.now
  override lazy val change = new EventSource[TransformedSeq[U]] {}
  private val changeListener: TransformedSeq[U]=>Unit = change.fire _
  private val deltasListener: SeqDelta[U,U]=>Unit = deltas.fire _
  private var currentMappedSignal = f(parent.now)
//  private var lastDeltas: Seq[SeqDelta[T,U]] = now.zipWithIndex.map{case (e,i)=>Include(i,e)}
  private var lastSeq: Seq[U] = now
  currentMappedSignal.change addListener changeListener
  currentMappedSignal.deltas addListener deltasListener
  
  private val parentChangeListener: T=>Unit = { x =>
    currentMappedSignal.change removeListener changeListener
    currentMappedSignal.deltas removeListener deltasListener
    currentMappedSignal = f(x)
    val n = currentMappedSignal.transform
    change.fire(n)
    lastSeq = fireDeltaDiff(lastSeq, n)
    currentMappedSignal.change addListener changeListener
    currentMappedSignal.deltas addListener deltasListener
  }
  parent.change addListener parentChangeListener
  
  private def fireDeltaDiff(lastSeq: Seq[U], newSeq: Seq[U]): Seq[U] = {
    deltas fire Batch(LCS.lcsdiff(lastSeq, newSeq, (_:U) == (_:U)): _*)
    newSeq
  }
  
  override def toString = "FlatMappedSeqSignal("+parent+","+System.identityHashCode(f)+")"
}



/**  Mix in this trait to a SeqSignal to have it fire change events  whenever a delta is fired.  Note that you must make sure ''observing'' is initialized  before this trait's body is executed.
 */
trait ChangingSeqSignal[T] extends SeqSignal[T] {
  private lazy val change0 = new EventSource[TransformedSeq[T]] {}
  override lazy val change: EventStream[TransformedSeq[T]] = change0
  private val fireTransform: SeqDelta[T,T]=>Unit = _ => change0 fire transform //TODO transform? underlying? now?
  deltas addListener fireTransform
}

