package reactive

import scala.collection.mutable.{
  Buffer,
  ArrayBuffer,
  Subscriber,
  Publisher
}

sealed trait Message[+A, +B] {
  def inverse: Message[B, A]
}
case class Include[+B](index: Int, elem: B) extends Message[Nothing, B] {
  def inverse = Remove(index, elem)
}
case class Update[+A, +B](index: Int, old: A, elem: B) extends Message[A, B] {
  def inverse = Update(index, elem, old)
}
case class Remove[+A](index: Int, old: A) extends Message[A, Nothing] {
  def inverse = Include(index, old)
}
case class Batch[+A, +B](messages: Message[A, B]*) extends Message[A, B] {
  def inverse = Batch(messages map {_.inverse} reverse: _*)
  def flatten: Seq[Message[A,B]] = messages.flatMap {
    case b: Batch[A,B] => b.flatten
    case m => List(m)
  }
}



class ObservableBuffer[T] extends ArrayBuffer[T] {
  lazy val messages = new Batchable[T,T] with Suppressable[Message[T,T]] {}
  
  override def +=(element: T): this.type = {
    super.+=(element)
    messages fire Include(length-1, element)
    this
  }
  
  override def +=:(element: T): this.type = {
    super.+=:(element)
    messages fire Include(0, element)
    this
  }

  override def update(n: Int, newelement: T): Unit = {
    val oldelement = apply(n)
    super.update(n, newelement)
    messages fire Update(n, oldelement, newelement)
  }

  override def remove(n: Int): T = {
    val oldelement = apply(n)
    super.remove(n)
    messages fire Remove(n, oldelement)
    oldelement
  }

  override def clear(): Unit = {
    val old = toList
    super.clear
    messages fire Batch(old.zipWithIndex.map{case (e,i)=>Remove(i,e)}: _*)
  }
  
  def applyDelta: Message[T,T]=>Unit = {
    case Include(i, e) => insert(i, e)
    case Remove(i, e) => remove(i)
    case Update(i, old, e) => update(i, e)
    case Batch(ms @ _*) => messages.batching {ms foreach applyDelta}
  }
}

trait SeqSignal[T] extends Signal[Seq[T]] {
  //private def wrapMapping[U](f: Seq[T]=>Seq[U]): Seq[T]=>Seq[U] = {
  //  _ => f(transform)
  //}
  /* protected  */class MappedSeqSignal[U](
    f: TransformedSeq[T]=>TransformedSeq[U]
  )(
    override implicit val observing: Observing
  ) extends ChangingSeqSignal[U] {
    import scala.ref.WeakReference
    private val emptyCache = new WeakReference[Option[TransformedSeq[U]]](None)
    protected var cached = emptyCache
    private var cache2: AnyRef = null
    protected def cache(v: TransformedSeq[U]): TransformedSeq[U] = {
      cached = new WeakReference(Some(v))
      cache2 = v
      v
    }
    def underlying: TransformedSeq[U] = (if(cached==null)None else cached.get) match {
      case None | Some(None) =>
        cache(f(SeqSignal.this.transform))
      case Some(Some(ret)) => ret
    }
    override lazy val change: EventStream[Seq[U]] = SeqSignal.this.change.map {
      case s: TransformedSeq[T] =>
//        println("change")
        cache(f(s))
    }
    def now = underlying.toList
    override def transform = underlying
    //change foreach {
    //  case s: TransformedSeq[U] => cache(s)
    //}
    override lazy val deltas = new EventStream[Message[U,U]] {} /* change.flatMap(underlying){
      case s: TransformedSeq[U] =>
	    println(this + " changing deltas")
	    s.deltas
    }*/
    SeqSignal.this.deltas.foreach{m =>
      underlying match {
        case t: TransformedSeq[T]#Transformed[U] => t.xform(m) match {
          case Nil =>
          case single :: Nil => deltas fire single
          case s => deltas fire Batch(s: _*)
        }
        case _ =>
      }
    }
  }
  private lazy val underlying = new TransformedSeq[T] {
    def underlying = SeqSignal.this.now
    def observing = SeqSignal.this.observing
  }
  def transform = underlying
  protected implicit def observing: Observing // TODO -- ???
  
  override lazy val change = new EventStream[Seq[T]] {}
  def deltas = transform.deltas
  
  def map[U](f: TransformedSeq[T]=>TransformedSeq[U])(implicit observing: Observing): SeqSignal[U] =
    new MappedSeqSignal[U](f)
    
  
}
object SeqSignal {
  implicit def apply[T](orig: Signal[Seq[T]])(implicit o: Observing): SeqSignal[T] =
    new Var(orig.now) with DiffSeqSignal[T] {
      orig.change foreach { seq =>
        update(new TransformedSeq[T] {
          def underlying = seq
          def observing = o
        })
      }
      def observing = o
    }
}

/**
  Mix in this trait to a SeqSignal to have it fire change events
  whenever a delta is fired
*/
trait ChangingSeqSignal[T] extends SeqSignal[T] {
  deltas foreach {_ => change.fire(transform)}
}

/**
  This SeqSignal contains a Buffer which you can modify
  directly, causing deltas to be fired.
  @see ChangingSeqSignal
*/
trait BufferSignal[T] extends SeqSignal[T] with ChangingSeqSignal[T] {
  protected lazy val underlying = new ObservableBuffer[T]
  def now: Buffer[T] = underlying
  def comparator: (T,T) => Boolean = {_ == _}
  final def value = now
  /**
    Set the contents from another Seq. Does not set it directly;
    rather calculates the diff and applies it.
  */
  def value_=(v: Seq[T]) {
    underlying applyDelta Batch(LCS.lcsdiff(now, v, comparator): _*)
  }
  final def update(v: Seq[T]) = value = v
  override lazy val transform = new TransformedSeq[T] {
    println("BufferSignal.transform: " + uid)
    def underlying = BufferSignal.this.underlying
    def observing = BufferSignal.this.observing
    override lazy val deltas = underlying.messages
  }
}

/**
  Whenever this SeqSignal's change EventStream fires, it calculates the
  deltas by diffing the old Seq and the new one.
  Normally you would mix this in to a Var.
*/
@deprecated("Instead we need a DiffSignal that extracts deltas from diffs")
trait DiffSeqSignal[T] extends SeqSignal[T] {this: Var[Seq[T]] =>
  def comparator: (T,T) => Boolean = {_ == _}
  override lazy val transform = new TransformedSeq[T] {
    def underlying = DiffSeqSignal.this.now
    def observing = DiffSeqSignal.this.observing
    //override val deltas = new EventStream[Message[T]] {}
  }
  change.foldLeft(value) {case (_prev, _cur) =>
    transform.deltas.fire(Batch(LCS.lcsdiff(_prev,_cur, comparator): _*))
    _cur
  }
}

/**
  This trait provides a SeqSignal that fires deltas on change, and
*/
@deprecated("Instead we need a BufferVar that fires change on mutate; and a DiffSignal that extracts deltas from diffs")
trait DiffBufferSignal[T] extends DiffSeqSignal[T] {this: Var[Seq[T]] =>
  protected var underlying = new ObservableBuffer[T]
  underlying.messages.suppressing {
    underlying.clear
    underlying ++= value
  }
  override def now: Buffer[T] = underlying
  private val fromBuffer = new scala.util.DynamicVariable(false)
  
  underlying.messages foreach {m =>
    fromBuffer.withValue(true) {
      transform.deltas.fire(m)
    }
  }
  private def applyDelta: Message[T,T]=>Unit = {
    case Include(i, e) =>
      underlying.messages.suppressing {
        underlying.insert(i, e)
      }
    case Remove(i, e) =>
      underlying.messages.suppressing {
        underlying.remove(i)
      }
    case Batch(ms @ _*) => ms foreach applyDelta
  }
  transform.deltas.filter(_ => !fromBuffer.value) foreach applyDelta
}


class DiffSignal[T](
  signal: SignalBase[Seq[T]],
  comparator: (T,T) => Boolean = {(_:T) == (_:T)}
)(
  implicit _observing: Observing
) extends SeqSignal[T] {
  protected var _now = signal.now
  def observing = _observing
  def now = _now
  signal.change.foldLeft(_now) {case (prev, cur) =>
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

