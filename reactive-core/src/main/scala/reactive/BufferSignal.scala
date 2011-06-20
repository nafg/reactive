package reactive



/**  This SeqSignal contains a Buffer which you can modify  directly, causing deltas to be fired.  You can also replace the buffer contents directly,  causing a diff to be calculated and the resulting deltas  to be fired.  @see ChangingSeqSignal
 */
trait BufferSignal[T] extends SeqSignal[T] with ChangingSeqSignal[T] {
  protected lazy val underlying = new ObservableBuffer[T]
  def now = transform
  /**
   * Override this to customize the comparator used
   * by the diff algorithm
   */
  def comparator: (T, T) => Boolean = { _ == _ }
  def value: scala.collection.mutable.ArrayBuffer[T] = underlying
  /**
   * Set the contents from another Seq. Does not set it directly;
   * rather calculates the diff and applies it.
   * Usage: bufferSignal.value = newContentsSeq
   */
  def value_=(v: Seq[T]) {
    val diff = Batch(LCS.lcsdiff(now, v, comparator): _*)
    underlying applyDelta diff
  }
  /**
   * Usage: bufferSignal ()= newContentsSeq
   */
  final def update(v: Seq[T]) = value = v
  override lazy val transform = new TransformedSeq[T] {
    def underlying = BufferSignal.this.underlying
    override lazy val deltas = underlying.messages
  }

  override def toString = "BufferSignal("+now+")"
}
object BufferSignal {
  def apply[T](init: T*): BufferSignal[T] = new BufferSignal[T] {
    value = init
  }
}

/**
 * Whenever this SeqSignal's change EventStream fires, it calculates the
 * deltas by diffing the old Seq and the new one.
 * Normally you would mix this in to a Var.
 */
@deprecated("Instead we need a DiffSignal that extracts deltas from diffs")
trait DiffSeqSignal[T] extends SeqSignal[T] { this: Var[TransformedSeq[T]] =>
  def comparator: (T, T) => Boolean = { _ == _ }
  override lazy val transform = new TransformedSeq[T] {
    def underlying = DiffSeqSignal.this.now
    //override val deltas = new EventStream[SeqDelta[T]] {}
  }
  change.foldLeft(value) {
    case (_prev, _cur) =>
      transform.deltas.fire(Batch(LCS.lcsdiff(_prev, _cur, comparator): _*))
      _cur
  }
}

/**  This trait provides a SeqSignal that fires deltas on change, and
 */
@deprecated("Instead we need a BufferVar that fires change on mutate; and a DiffSignal that extracts deltas from diffs")
trait DiffBufferSignal[T] extends DiffSeqSignal[T] { this: Var[TransformedSeq[T]] =>
  protected val underlying = new ObservableBuffer[T]
  underlying.messages.suppressing {
    underlying.clear
    underlying ++= value
  }
  override def now = transform
  private val fromBuffer = new scala.util.DynamicVariable(false)

  private val propagate: SeqDelta[T,T]=>Unit = { m =>
    fromBuffer.withValue(true) {
      transform.deltas.fire(m)
    }
  }
  underlying.messages addListener propagate
  private def applyDelta: SeqDelta[T, T] => Unit = {
    case Include(i, e) =>
      underlying.messages.suppressing {
        underlying.insert(i, e)
      }
    case Remove(i, e) =>
      underlying.messages.suppressing {
        underlying.remove(i)
      }
    case Batch(ms@_*) => ms foreach applyDelta
  }
  private lazy val o = new Observing {}
  transform.deltas.filter(_ => !fromBuffer.value).foreach(applyDelta)(o)
}

class DiffSignal[T](
  signal: Signal[TransformedSeq[T]],
  comparator: (T, T) => Boolean = { (_: T) == (_: T) }
)(
  implicit _observing: Observing
) extends SeqSignal[T] with Logger {
  case class CalculatedDiff(prev: Seq[T], cur: Seq[T], diff: Seq[SeqDelta[T,T]]) extends LogEventPredicate
  
  protected var _now = signal.now
  def observing = _observing
  def now = _now
  signal.change.foldLeft(_now) {
    case (prev, cur) =>
      _now = cur
      val diff = LCS.lcsdiff(prev, cur, comparator)
      trace(CalculatedDiff(prev,cur,diff))
      transform.deltas.fire(Batch(diff: _*))
      cur
  }
  override def toString = "DiffSeqSignal"
}

/*
import scala.collection._
import scala.collection.generic._
trait MyLike[+A] extends Seq[A]
                 with GenericTraversableTemplate[A, MyColl] 
                 with SeqLike[A, MyColl[A]] { self=>
  protected def seq: Seq[A]
  override def companion = MyColl
  def iterator = seq.iterator
  def apply(i: Int) = seq(i)
  def length = seq.length
  /*override def view = new MyView[A] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }*/
}
class MyColl[+A](val seq: A*) extends MyLike[A]
object MyColl extends SeqFactory[MyColl] {  
//  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, MyColl[A]] = new GenericCanBuildFrom[A]
//  def newBuilder[A] = new scala.collection.mutable.LazyBuilder[A,MyColl[A]] {
//    def result = {
//      val data = parts.foldLeft(List[A]()){(l,n) => l ++ n}
//      new MyColl(data:_*)
//    }
//  }
//  def newBuilder[T] = new Builder[T, MyColl[T]] {
//    var list = List[T]()
//    def result = list
//    def clear { list = Nil}
//    def +=(elem: T) = list += elem
//  }
}*/

//trait MyViewLike[+A,+Coll,+This<:MyView[A] with MyViewLike[A,Coll,This]]
//extends MyColl[A] with SeqViewLike[A, Coll, This] {
//  trait Transformed[B] extends SeqView[B, MyColl[B]] with super.Transformed[B]
//  trait Forced[B] extends Transformed[B] with super.Forced[B]
//  trait Sliced extends Transformed[A] with super.Sliced
//  trait Mapped[B] extends Transformed[B] with super.Mapped[B]
//  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B]
//  trait Appended[B] extends Transformed[B] with super.Appended[B]
//  trait Filtered extends Transformed[A] with super.Filtered
//  trait TakenWhile extends Transformed[A] with super.TakenWhile
//  trait DroppedWhile extends Transformed[A] with super.DroppedWhile
//  trait Zipped[B] extends Transformed[(A, B)] with super.Zipped[B]
//  trait ZippedAll[A1 >: A, B] extends Transformed[(A1, B)] with super.ZippedAll[A1, B]
//  trait Reversed extends Transformed[A]
//  trait Patched[B >: A] extends Transformed[B]
//  trait Prepended[B >: A] extends Transformed[B]
//  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
//  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
//  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
//  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
//  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
//  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
//  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
//  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
//  protected override def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new Zipped[B] { val other = that }
//  protected override def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new ZippedAll[A1, B] { val other = that; val thisElem = _thisElem; val thatElem = _thatElem }
//  protected override def newReversed: Transformed[A] = new Reversed { }
//  protected override def newPatched[B >: A](_from: Int, _patch: Seq[B], _replaced: Int): Transformed[B] = new Patched[B] { val from = _from; val patch = _patch; val replaced = _replaced }
//  protected override def newPrepended[B >: A](elem: B): Transformed[B] = new Prepended[B] { protected[this] val fst = elem }
//}
//trait MyViewLike[+A,+Coll,+This<:MyView[A] with MyViewLike[A,Coll,This]]
//extends MyColl[A] with SeqViewLike[A, Coll, This]
//trait MyView[A] extends MyViewLike[A, MyColl[A], MyView[A]] with SeqView[A, MyColl[A]]
//object MyView {
//  type Coll = MyView[_]
//  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, MyView[A]] =
//    new CanBuildFrom[Coll, A, MyView[A]] { 
//      def apply(from: Coll) = new scala.collection.TraversableView.NoBuilder
//      def apply() = new scala.collection.TraversableView.NoBuilder
//    }
//}

