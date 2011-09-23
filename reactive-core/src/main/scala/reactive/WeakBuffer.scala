package reactive

import scala.collection.{ SeqProxy, SeqLike, LinearSeqOptimized, IterableProxyLike }
import scala.collection.generic._
import scala.collection.immutable.LinearSeq
import scala.collection.script._
import scala.ref.WeakReference
import scala.collection.mutable.BufferProxy
import scala.collection.mutable.Buffer
import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.ref.ReferenceQueue

/**
 * Based on scala.collection.mutable.BufferProxy
 */
class WeakBuffer[A <: AnyRef] extends Buffer[A] {

  val self = new ListBuffer[WeakReference[A]]

  override def iterator: Iterator[A] = self.toList.iterator.flatMap(_.get.toIterator)

  final def compact(i: Int): Unit = if (i < self.length) {
    if (self(i).get.isDefined) compact(i + 1)
    else {
      self.remove(i)
      compact(i)
    }
  }

  private def ref(x: A): WeakReference[A] = new WeakReference(x)

  def apply(n: Int): A = {
    var ret = self.apply(n).get
    while (ret.isEmpty) {
      self.remove(n)
      ret = self.apply(n).get
    }
    ret.get
  }
  
  def length = {
    compact(0)
    self.length
  }

  def update(n: Int, newelem: A) { self.update(n, ref(newelem)); compact(n) }

  def remove(n: Int): A = {
    val ret = apply(n)
    self.remove(n)
    compact(n)
    ret
  }

  override def +(elem: A): Buffer[A] = { self += ref(elem); this }

  def +=(elem: A): this.type = { self += ref(elem); this }

  override def readOnly = self.readOnly.flatMap(_.get)

  override def ++(xs: TraversableOnce[A]): Buffer[A] = { xs foreach this.+; this }

  override def ++=(xs: TraversableOnce[A]): this.type = { xs foreach this.+=; this }

  override def append(elems: A*) { ++=(elems) }

  override def appendAll(xs: TraversableOnce[A]) { xs foreach { x => append(x) } }

  def +=:(elem: A): this.type = { self.+=:(ref(elem)); this }

  override def ++=:(xs: TraversableOnce[A]): this.type = { xs foreach +=:; this }

  override def prepend(elems: A*) { self.prependAll(elems map ref) }

  override def prependAll(xs: TraversableOnce[A]) { xs foreach { x => prepend(x) } }

  override def insert(n: Int, elems: A*) { self.insertAll(n, elems map ref) }

  def insertAll(n: Int, iter: scala.collection.Iterable[A]) { self.insertAll(n, iter map ref) }

  override def insertAll(n: Int, iter: scala.collection.Traversable[A]) { self.insertAll(n, iter map ref) }

  def clear() { self.clear }

  override def <<(cmd: Message[A]) = throw new RuntimeException("WeakBuffer.<< not implemented")

  override def clone(): Buffer[A] = new WeakBuffer[A] {
    override val self = WeakBuffer.this.self.clone()
    compact(0)
  }
}

class WeakList[A](xs: WeakReference[A with AnyRef]*) extends LinearSeq[A]
  with GenericTraversableTemplate[A, WeakList]
  with LinearSeqOptimized[A, WeakList[A]] {

  override def companion: GenericCompanion[WeakList] = WeakList

  private var list: Seq[WeakReference[A with AnyRef]] = xs

  def underlying = list

  def compact: this.type = {
    if (list.nonEmpty && list.exists(_.get.isEmpty)) list.synchronized{
      list = list.filter(_.get.isDefined)
    }
    this
  }

  override def isEmpty = { list.isEmpty || list.forall(_.get.isEmpty) }
  override def head = { list.head.get getOrElse tail.head }
  override def tail = { new WeakList(list.tail: _*) }

  def :+[B >: A <: AnyRef](elem: B): WeakList[B] = new WeakList(list :+ new WeakReference(elem): _*)
}

object WeakList extends SeqFactory[WeakList] {
  implicit def canBuildFrom[A <: AnyRef]: CanBuildFrom[Coll, A, WeakList[A]] = new GenericCanBuildFrom[A]

  def newBuilder[A]: Builder[A, WeakList[A]] = new Builder[A, WeakList[A]] {
    var list = List[A with AnyRef]()
    def result = new WeakList(list.map(x => new WeakReference(x)): _*)
    def clear { list = Nil }
    def +=(elem: A) = { list :+= elem.asInstanceOf[A with AnyRef]; this }
  }
}
