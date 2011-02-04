package reactive

import scala.collection.mutable.ArrayBuffer


/**
 * A Buffer that contains an EventStream which fires SeqDelta events
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
  lazy val messages = new Batchable[T, T] with Suppressable[SeqDelta[T, T]] {}

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
      newElements.toSeq.zipWithIndex.map { case (e, i) => Include(n + i, e) }: _*)
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
   * Mutates this buffer by applying a SeqDelta to it.
   * To keep two ObservableBuffers in sync, you could write
   * buffer1.messages foreach buffer2.applyDelta
   */
  def applyDelta: SeqDelta[T, T] => Unit = {
    case Include(i, e) => insert(i, e)
    case Remove(i, e) => remove(i)
    case Update(i, old, e) => update(i, e)
    case Batch(ms@_*) => messages.batching { ms foreach applyDelta }
  }
}
