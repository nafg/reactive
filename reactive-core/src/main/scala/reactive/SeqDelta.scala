package reactive

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.collection.mutable.Buffer

/**
 * Represents the delta of a change to a Seq (such as a Buffer)
 * @tparam A the type of the old element
 * @tparam B the type of the new element
 */
object SeqDelta {
  def flatten[A, B](deltas: Seq[SeqDelta[A, B]]): Seq[SingleDelta[A, B]] =
    recursively(deltas.toList)(List.empty[SingleDelta[A, B]])(_ :+ _)

  def single[A, B](ms: Seq[SeqDelta[A, B]]): Option[SeqDelta[A, B]] =
    if (ms.isEmpty) None
    else if (ms.length == 1) Some(ms(0))
    else Some(Batch(ms: _*))

  def applyToSeq[A](ds: Seq[SeqDelta[A, A]])(seq: Seq[A]): Seq[A] = {
    val buf = ArrayBuffer(seq: _*)
    applyToBuffer(ds, buf)
    buf.toSeq
  }
  def applyToBuffer[A](ds: Seq[SeqDelta[A, A]], buf: Buffer[A]): Unit = flatten(ds) foreach {
    case Include(i, e)     => buf.insert(i, e)
    case Remove(i, e)      => buf.remove(i)
    case Update(i, old, e) => buf.update(i, e)
  }

  @tailrec def recursively[A, B, C](deltas: List[SeqDelta[A, B]])(acc: C)(f: (C, SingleDelta[A, B]) => C): C = deltas match {
    case Nil                              => acc
    case Batch(ds @ _*) :: rest           => recursively(ds.toList ++ rest)(acc)(f)
    case (one: SingleDelta[A, B]) :: rest => recursively(rest)(f(acc, one))(f)
  }
  def patch[U](seq: Seq[U], delta: SeqDelta[U, U]): Seq[U] = recursively(delta :: Nil)(seq){
    case (s, Include(n, e))   => s.patch(n, List(e), 0)
    case (s, Remove(n, _))    => s.patch(n, Nil, 1)
    case (s, Update(n, _, e)) => s.patch(n, List(e), 1)
  }

  final class Batchable[A, B](source: Seq[SeqDelta[A, B]]) {
    def toBatch: Batch[A, B] = Batch(source: _*)
  }
  implicit def seqToBatchable[A, B](source: Seq[SeqDelta[A, B]]): Batchable[A, B] = new Batchable(source)
}
sealed trait SeqDelta[+A, +B] {
  /**
   * The message that, if applied, would undo the result of this message
   */
  def inverse: SeqDelta[B, A]
}

sealed trait SingleDelta[+A, +B] extends SeqDelta[A, B] {
  def index: Int
}
sealed trait IncludeOrRemove[+A, +B] extends SingleDelta[A, B]

/**
 * Represents an insertion at an index
 */
//TODO maybe rename to Insert?
case class Include[+B](index: Int, elem: B) extends IncludeOrRemove[Nothing, B] {
  def inverse = Remove(index, elem)
}
/**
 * Represents an element being replaced at an index.
 */
//TODO maybe rename to Replace?
case class Update[+A, +B](index: Int, old: A, elem: B) extends SingleDelta[A, B] {
  def inverse = Update(index, elem, old)
}
/**
 * Represents an element being removed at an index
 */
case class Remove[+A](index: Int, old: A) extends IncludeOrRemove[A, Nothing] {
  def inverse = Include(index, old)
}
/**
 * Represents a batch of SeqDeltas.
 * Can be used to roll together a number of SeqDeltas
 * so that they will be applied in one go, which is often
 * more efficient. For instance if after every change
 * something needs to be updated, by using a Batch
 * the update can be deferred until after the entire
 * set of changes are applied.
 * @param messages the messages contained in the batch
 */
case class Batch[+A, +B](messages: SeqDelta[A, B]*) extends SeqDelta[A, B] {
  def inverse = Batch(messages map { _.inverse } reverse: _*)
  /**
   * Returns the messages as a Seq of SeqDeltas that does not contain
   * any batches.
   */
  def flatten = SeqDelta.flatten(messages)
  def applyToSeq[T >: A](seq: Seq[T]) = SeqDelta.applyToSeq(messages)(seq)
}
