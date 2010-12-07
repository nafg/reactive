package reactive

import scala.collection.mutable.{
  Buffer,
  ArrayBuffer
}

/**
 * Represents the delta of a change to a Seq (such as a Buffer)
 * @tparam A the type of the old element
 * @tparam B the type of the new element
 */
//TODO rename to Delta or SeqDelta
sealed trait Message[+A, +B] {
  /**
   * The message that, if applied, would undo the result of this message
   */
  def inverse: Message[B, A]
}
/**
 * Represents an insertion at an index
 */
//TODO maybe rename to Insert?
case class Include[+B](index: Int, elem: B) extends Message[Nothing, B] {
  def inverse = Remove(index, elem)
}
/**
 * Represents an element being replaced at an index.
 */
//TODO maybe rename to Replace?
case class Update[+A, +B](index: Int, old: A, elem: B) extends Message[A, B] {
  def inverse = Update(index, elem, old)
}
/**
 * Represents an element being removed at an index
 */
case class Remove[+A](index: Int, old: A) extends Message[A, Nothing] {
  def inverse = Include(index, old)
}
/**
 * Represents a batch of Messages.
 * Can be used to roll together a number of Messages
 * so that they will be applied in one go, which is often
 * more efficient. For instance if after every change
 * something needs to be updated, by using a Batch
 * the update can be deferred until after the entire
 * set of changes are applied.
 * @param messages the messages contained in the batch
 */
case class Batch[+A, +B](messages: Message[A, B]*) extends Message[A, B] {
  def inverse = Batch(messages map { _.inverse } reverse: _*)
  /**
   * Returns the messages as a Seq of Messages that does not contain
   * any batches.
   */
  def flatten: Seq[Message[A, B]] = messages.flatMap {
    case b: Batch[A, B] => b.flatten
    case m => List(m)
  }
}
object Batch {
  def single[A, B](ms: Seq[Message[A, B]]): Option[Message[A, B]] =
    if (ms.isEmpty) None
    else if (ms.length == 1) Some(ms(0))
    else Some(Batch(ms: _*))
}

/**
 * A Buffer that contains an EventStream which fires Message events
 * after every time the Buffer is updated.
 */
//TODO Should this really be a trait mixable into any Buffer?
//Possible reason not is if different implementations implement
//different operations in terms of different other operations.
//For example if += is implemented in terms of insertAll in one
//Buffer but not in the other, then the only way to prevent
//deltas from being fired twice is to wrap the call to super
//in 'suppressing,' slowing things down.
class ObservableBuffer[T] extends ArrayBuffer[T] {
  /**
   * An EventStream that fires events after each buffer mutation
   */
  //TODO rename to deltas
  lazy val messages = new Batchable[T, T] with Suppressable[Message[T, T]] {}

  override def +=(element: T): this.type = {
    super.+=(element)
    messages fire Include(length - 1, element)
    this
  }

  override def +=:(element: T): this.type = {
    super.+=:(element)
    messages fire Include(0, element)
    this
  }

  override def insertAll(n: Int, newElements: Traversable[T]): Unit = {
    super.insertAll(n, newElements)
    messages fire Batch(
      newElements.toSeq.zipWithIndex.map { case (e, i) => Include(n + i, e) }: _*
      )
  }

  override def update(n: Int, newelement: T): Unit = {
    val oldelement = apply(n)
    super.update(n, newelement)
    messages fire Update(n, oldelement, newelement)
  }

  override def remove(index: Int): T = {
    val oldelement = apply(index)
    super.remove(index)
    messages fire Remove(index, oldelement)
    oldelement
  }

  override def clear(): Unit = {
    val old = toList
    super.clear
    messages fire Batch(old map { e => Remove(0, e) }: _*)
  }

  /**
   * Mutates this buffer by applying a Message to it.
   * To keep two ObservableBuffers in sync, you could write
   * buffer1.messages foreach buffer2.applyDelta
   */
  def applyDelta: Message[T, T] => Unit = {
    case Include(i, e) => insert(i, e)
    case Remove(i, e) => remove(i)
    case Update(i, old, e) => update(i, e)
    case Batch(ms@_*) => messages.batching { ms foreach applyDelta }
  }
}

/**
 * This trait provides special behavior implementations for signals of sequences,
 * preserving the transformation relationship of derived signals by propagating
 * deltas (Messages).
 */
//class SeqSignal[A](seq: TransformedSeq[A]) extends Signal[TransformedSeq[A]] with SignalLike[TransformedSeq[A], SeqSignal[A]] {
//  def this(seq: Seq[A]) = this(TransformedSeq(seq: _*))
//  def content = seq
//  def map[B, To](f: TransformedSeq[A] => B)(implicit mapper: SignalMapper[SeqSignal[A], TransformedSeq[A], B, To]): To = mapper(this, f)
//  override def toString = seq.mkString("SeqSignal(", ", ", ")")
//}

//object SeqSignal {
//  def apply[A](values: A*) = new SeqSignal(TransformedSeq(values: _*))
//}


trait SeqSignal[T] extends SimpleSignal[Seq[T]] {
  
  //private def wrapMapping[U](f: Seq[T]=>Seq[U]): Seq[T]=>Seq[U] = {
  //  _ => f(transform)
  //}
  private lazy val underlying = new TransformedSeq[T] {
    def underlying = SeqSignal.this.now
  }
  /**
   * Returns the TransformedSeq, the actual delta-propagating Seq.
   */
  def transform: TransformedSeq[T] = underlying

  override lazy val change: EventStream[Seq[T]] = new EventSource[Seq[T]] {}

  /**
   * The EventStream of incremental updates (Messages) to the underlying Seq.
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
  implicit def apply[T](orig: Signal[TransformedSeq[T]]): SeqSignal[T] =
    new Var[Seq[T]](orig.now) with DiffSeqSignal[T] {
      val origChange = orig.change
      origChange addListener { seq =>
        update(new TransformedSeq[T] {
          def underlying = seq
        })
      }
    }
}

class MappedSeqSignal[T,E, U <: TransformedSeq[E]](
  parent: Signal[T],    
  f: T => U
) extends ChangingSeqSignal[E] {
  import scala.ref.WeakReference
  private val emptyCache = new WeakReference[Option[U]](None)
  protected var cached = emptyCache
  private var cache2: AnyRef = null
  protected def cache(v: U): U = {
    cached = new WeakReference(Some(v))
    cache2 = v
    v
  }
  def underlying: U = (if (cached == null) None else cached.get) match {
    case None | Some(None) =>
      cache(f(parent.now))
    case Some(Some(ret)) => ret
  }
  override lazy val change: EventStream[Seq[E]] = parent.change.map {
    case s: T =>
      //        println("change")
      cache(f(s))
  }
  def now = underlying
  override def transform = underlying
  //change foreach {
  //  case s: TransformedSeq[U] => cache(s)
  //}
  override lazy val deltas = new EventSource[Message[E, E]] {} /* change.flatMap(underlying){
    case s: TransformedSeq[U] =>
    println(this + " changing deltas")
    s.deltas
  }*/
  parent match {
  case ss: SeqSignal[T] =>
    println("parent is a SeqSignal")
    val parentDeltas = ss.deltas
    parentDeltas.addListener { m =>
      underlying match {
        case t: TransformedSeq[T]#Transformed[E] =>
          Batch.single(t.xform(m)) foreach deltas.fire
        case _ =>
      }
    }
  case _ =>
    println("parent is not a SeqSignal")
  }
}


/**  Mix in this trait to a SeqSignal to have it fire change events  whenever a delta is fired.  Note that you must make sure ''observing'' is initialized  before this trait's body is executed.
 */
trait ChangingSeqSignal[T] extends SeqSignal[T] {
  private lazy val change0 = new EventSource[Seq[T]] {}
  override lazy val change: EventStream[Seq[T]] = change0
  deltas addListener { _ =>
    //    println("Received delta, firing change")
    change0.fire(transform) //TODO transform? underlying? now?
    //    println("Fired change")
  }
}

/**  This SeqSignal contains a Buffer which you can modify  directly, causing deltas to be fired.  You can also replace the buffer contents directly,  causing a diff to be calculated and the resulting deltas  to be fired.  @see ChangingSeqSignal
 */
trait BufferSignal[T] extends SeqSignal[T] with ChangingSeqSignal[T] {
  protected lazy val underlying = new ObservableBuffer[T]
  def now: Buffer[T] = underlying
  /**
   * Override this to customize the comparator used
   * by the diff algorithm
   */
  def comparator: (T, T) => Boolean = { _ == _ }
  final def value = now
  /**
   * Set the contents from another Seq. Does not set it directly;
   * rather calculates the diff and applies it.
   * Usage: bufferSignal.value = newContentsSeq
   */
  def value_=(v: Seq[T]) {
    val diff = Batch(LCS.lcsdiff(now, v, comparator): _*)
    //    println("Value changed, applying diff: " + diff)
    underlying applyDelta diff
    //    println("Applied diff")
  }
  /**
   * Usage: bufferSignal ()= newContentsSeq
   */
  final def update(v: Seq[T]) = value = v
  override lazy val transform = new TransformedSeq[T] {
    def underlying = BufferSignal.this.underlying
    override lazy val deltas = underlying.messages
  }
}
object BufferSignal {
  def apply[T](init: T*): BufferSignal[T] = new BufferSignal[T] {
    value = init
  }
}

/**
 * Whenever this SeqSignal's change EventStream fires, it calculates the  deltas by diffing the old Seq and the new one.  Normally you would mix this in to a Var.
 */
@deprecated("Instead we need a DiffSignal that extracts deltas from diffs")
trait DiffSeqSignal[T] extends SeqSignal[T] { this: Var[Seq[T]] =>
  def comparator: (T, T) => Boolean = { _ == _ }
  override lazy val transform = new TransformedSeq[T] {
    def underlying = DiffSeqSignal.this.now
    //override val deltas = new EventStream[Message[T]] {}
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
trait DiffBufferSignal[T] extends DiffSeqSignal[T] { this: Var[Seq[T]] =>
  protected val underlying = new ObservableBuffer[T]
  underlying.messages.suppressing {
    underlying.clear
    underlying ++= value
  }
  override def now: Buffer[T] = underlying
  private val fromBuffer = new scala.util.DynamicVariable(false)

  underlying.messages addListener { m =>
    fromBuffer.withValue(true) {
      transform.deltas.fire(m)
    }
  }
  private def applyDelta: Message[T, T] => Unit = {
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
  comparator: (T, T) => Boolean = { (_: T) == (_: T) })(
  implicit _observing: Observing) extends SeqSignal[T] {
  protected var _now = signal.now
  def observing = _observing
  def now = _now
  signal.change.foldLeft(_now) {
    case (prev, cur) =>
      _now = cur
      val diff = LCS.lcsdiff(prev, cur, comparator)
      println(prev + " DIFF " + cur + " = " + diff)
      transform.deltas.fire(Batch(diff: _*))
      cur
  }
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

