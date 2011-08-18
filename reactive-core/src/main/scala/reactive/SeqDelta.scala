package reactive

import scala.collection.mutable.ArrayBuffer

/**
 * Represents the delta of a change to a Seq (such as a Buffer)
 * @tparam A the type of the old element
 * @tparam B the type of the new element
 */
object SeqDelta {
  def flatten[A, B](messages: Seq[SeqDelta[A, B]]): Seq[SeqDelta[A, B]] = messages.flatMap {
    case b: Batch[A, B] => flatten(b.messages)
    case m              => List(m)
  }
  def single[A, B](ms: Seq[SeqDelta[A, B]]): Option[SeqDelta[A, B]] =
    if (ms.isEmpty) None
    else if (ms.length == 1) Some(ms(0))
    else Some(Batch(ms: _*))

  def applyToSeq[A](messages: Seq[SeqDelta[A, A]])(seq: Seq[A]): Seq[A] = {
    val buf = ArrayBuffer(seq: _*)
    def applyDelta(m: SeqDelta[A, A]): Unit = m match {
      case Include(i, e)     => buf.insert(i, e)
      case Remove(i, e)      => buf.remove(i)
      case Update(i, old, e) => buf.update(i, e)
      case Batch(ms@_*)      => ms foreach applyDelta
    }
    messages foreach applyDelta
    buf.toSeq
  }
  final class Batchable[A, B](source: Seq[SeqDelta[A, B]]) {
    def toBatch: Batch[A, B] = Batch(source: _*)
  }
  implicit def seqToBatchable[A, B](source: Seq[SeqDelta[A, B]]) = new Batchable(source)
}
sealed trait SeqDelta[+A, +B] {
  /**
   * The message that, if applied, would undo the result of this message
   */
  def inverse: SeqDelta[B, A]
}
sealed trait IncludeOrRemove[+A, +B] extends SeqDelta[A, B]
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
case class Update[+A, +B](index: Int, old: A, elem: B) extends SeqDelta[A, B] {
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
