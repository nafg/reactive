package reactive

/**
 * Convenience class for a Var that is a SeqSignal.
 */
class SeqVar[A](init: A*) extends Var(DeltaSeq.fromSeq(init)) with SeqSignal[A]

/**
 * This SeqSignal contains a Buffer which you can modify directly,
 * causing deltas to be fired. You can also replace the buffer contents directly,
 * causing a diff to be calculated and the resulting deltas
 * to be fired.
 */
class BufferSignal[T] extends SeqSignal[T] {
  lazy val underlying = new ObservableBuffer[T]
  val change = new EventSource[DeltaSeq[T]]
  val now = new DeltaSeq[T] {
    def signal = BufferSignal.this
    val underlying = BufferSignal.this.underlying
    val fd = underlying.messages hold DeltaSeq.startDelta(underlying)
    def fromDelta = fd.now
  }
  private val dl: SeqDelta[T, T] => Unit = { _ =>
    change fire now
  }
  underlying.messages addListener dl

  /**
   * Override this to customize the comparator used
   * by the diff algorithm when updating the
   * value with a whole Seq.
   */
  def comparator: (T, T) => Boolean = { _ == _ }
  def value = underlying

  /**
   * Set the contents from another Seq. Does not set it directly;
   * rather calculates the diff and applies it.
   * Usage: bufferSignal.value = newContentsSeq
   */
  def value_=(v: Seq[T]): Unit = {
    val diff = Batch(LCS.lcsdiff(now, v, comparator): _*)
    underlying applyDelta diff
  }
  /**
   * Usage: bufferSignal ()= newContentsSeq
   */
  final def update(v: Seq[T]): Unit = value = v

  override def toString = "BufferSignal("+now+")"
}
object BufferSignal {
  def apply[T](init: T*): BufferSignal[T] = new BufferSignal[T] {
    value = init
  }
  implicit def canForwardTo[A]: CanForwardTo[BufferSignal[A], Seq[A]] = new CanForwardTo[BufferSignal[A], Seq[A]] {
    def forwarder(t: BufferSignal[A]) = NamedFunction(">>"+t.debugName)(t.update)
  }
}

