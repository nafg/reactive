package reactive

import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.SeqFactory
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.mutable.Builder
import scala.collection.immutable
import scala.collection.SeqLike
import Compat.GTraversableOnce

object DeltaSeq extends SeqFactory[DeltaSeq] {
  class DeltaSeqCBF[A] extends CanBuildFrom[DeltaSeq[_], A, DeltaSeq[A]] {
    def apply(from: Coll) = from match {
      case f: DeltaSeq[_] => new DeltaBuilder[A]
      case other          => from.genericBuilder[A]
    }
    def apply() = newBuilder[A]
  }
  implicit def canBuildFrom[T]: CanBuildFrom[Coll, T, DeltaSeq[T]] = new DeltaSeqCBF[T]

  def newBuilder[T] = new DeltaBuilder[T]
  class DeltaBuilder[T] extends Builder[T, DeltaSeq[T]] {
    var list = List[T]()
    def result = DeltaSeq.fromSeq(list.reverse)
    def clear { list = Nil }
    def +=(elem: T) = { list ::= elem; this }
  }

  sealed abstract class Transformed[+T, +V] extends DeltaSeq[V]
  class Appended[T](val parent: DeltaSeq[T], val xs: Seq[T]) extends Transformed[T, T] {
    val (underlying, fromDelta) = {
      val pu = parent.underlying
      (pu ++ xs, startDelta(pu ++ xs))
    }
  }
  class Folded[T, +V](val parent: DeltaSeq[T], val f: T => GTraversableOnce[V]) extends Transformed[T, V] {
    lazy val (underlying, indexMap) = {
      val buf = new ArrayBuffer[V]
      val map = scala.collection.mutable.Map.empty[Int, Range]
      var j = 0
      for ((x, i) <- parent.underlying.zipWithIndex) {
        val y = f(x)
        val yl = y.toList
        buf ++= yl
        map(i) = j until j + yl.size
        j += y.size
      }
      (buf.toSeq, map.toMap)
    }
    lazy val fromDelta: SeqDelta[V, V] = startDelta(underlying)
  }
  class Sliced[+T](val parent: DeltaSeq[T], val from: Int, val until: Int) extends Transformed[T, T] {
    lazy val underlying = parent.underlying.slice(from, until)
    lazy val fromDelta = startDelta(underlying)
  }
  class PrefixWhile[T](val parent: DeltaSeq[T], val take: Boolean, val p: T => Boolean) extends Transformed[T, T] {
    def filterIndex(lastValid: Int, i: Int): Option[Int] = if (take)
      Some(i) filter (_ <= lastValid)
    else
      Some(i - lastValid - 1) filter (_ => i > lastValid)
    lazy val valid = parent.underlying.toStream map { x => x -> p(x) }
    lazy val underlying = if (take)
      valid.takeWhile{ case (_, b) => b }.map(_._1)
    else
      valid.dropWhile{ case (_, b) => b }.map(_._1)
    lazy val fromDelta = startDelta(underlying)
  }

  def startDelta[A](xs: Seq[A], off: Int = 0): SeqDelta[A, A] = Batch(xs.zipWithIndex map { case (v, i) => Include(i + off, v) }: _*)
  def fromSeq[A](xs: Seq[A]) = new DeltaSeq[A] {
    val underlying = xs
    val fromDelta = startDelta(xs)
  }
  override def apply[A](xs: A*) = fromSeq(xs)
  implicit def toSeq[A](ds: DeltaSeq[A]): Seq[A] = ds.underlying

  def updatedByValue[T](prev: DeltaSeq[T], seq: Seq[T]): DeltaSeq[T] = new DeltaSeq[T] {
    val underlying = seq
    val fromDelta = Batch(LCS.lcsdiff[T, T](prev.underlying, seq, _ == _): _*)
  }
  def updatedByDeltas[T](prev0: DeltaSeq[T], delta: SeqDelta[T, T]): DeltaSeq[T] = new DeltaSeq[T] {
    val fromDelta = delta
    val underlying = SeqDelta.patch(prev0.underlying, fromDelta)
  }

  /**
   * Given a DeltaSeq meant to replace the parent DeltaSeq,
   * return a DeltaSeq that is equivalent to reapplying the
   * transformation this TransformedSeq was derived from to the replacement
   * TransformedSeq.
   * That is, given
   * val a: DeltaSeq[Int]            // 1, 2, 3
   * val b: DeltaSeq[Int]            // 2, 3 (fromDelta == Remove(0, 1))
   * val c: DeltaSeq[Int]            // 2, 3, 4 (fromDelta == Include(2, 4))
   * val x = a map (10 *)            // 10, 20, 30
   * val y = x.updatedFromParent(b)  // 20, 30 (fromDelta == Remove(0, 10))
   * val z = y.updatedFromParent(c)  // 20, 30, 40 (fromDelta == Include(2, 40))
   *
   * Ideally the TransformedSeq should be inferred from parentUpdated.fromDelta,
   * so in the above example each call to updatedFromParent only translates one delta.
   */
  /*
   * [1, 2, 3]                                 [2, 3](Remove(0,1))                         [2, 3, 4](Include(2, 4))
   *     |                                                  |                                         |
   *    \/                                                 \/                                        \/
   * flatMap(x => List(x*10,x*10+1).filter(_<41))  updatedFromParent                           updatedFromParent
   *     |                                                  |                                         |
   *    \/                                                 \/                                        \/
   * [0->(0..1), 1->(2..3), 2->(4..5)]         [0->(0..1), 1->(2..3)]                      [0->(0..1), 1->(2..3), 2->(4)]
   * [10,11, 20,21, 30,31]                     [20,21, 30,31](Remove(0,10),Remove(0,11))   [20,21, 30,31, 40](Include(4, 40))
   *
   */
  def updatedFromParent[T, V](prev: Transformed[T, V], parentUpdated: DeltaSeq[T]) = prev match {
    case prev0: Folded[T, V] => new Folded[T, V](parentUpdated, prev0.f) {
      override lazy val (fromDelta, indexMap) = {
        val uds = SeqDelta flatten List(parentUpdated.fromDelta)
        var map = prev0.indexMap
        val ds = new ArrayBuffer[SeqDelta[V, V]]
        def applyDelta(d: SingleDelta[T, T]): Unit = d match {
          case Remove(i, _) => // convert a remove on parent.prev to a remove on prev  (parent == parentUpdated)
            val prevFlatmappedIndices = prev0.indexMap(i)
            map = map - i map {
              case (j, xs) if j > i => (j - 1, xs)
              case other            => other
            }
            // all removes happen at same index
            ds ++= prevFlatmappedIndices map (i =>
              Remove(prevFlatmappedIndices.start, prev.underlying(i))
            )
          case Include(i, e) =>
            val startIndex = prev0.indexMap get i map (_.start) getOrElse prev.underlying.length
            val res = f(e)
            val resSeq = res.toList
            ds ++= resSeq.zipWithIndex map { case (v, j) => Include(startIndex + j, v) }
            map = map.map {
              case (j, xs) if j >= i => (j + 1, xs)
              case other             => other
            } + (i -> (startIndex until startIndex + resSeq.length))
          case Update(i, o, e) =>
            applyDelta(Remove(i, o))
            applyDelta(Include(i, e))
        }
        uds foreach applyDelta
        (Batch(ds.toSeq: _*), map)
      }
      override lazy val underlying = SeqDelta.patch(prev0.underlying, fromDelta)
    }
    case prev0: Appended[_] => new Appended[T](parentUpdated, prev0.xs.asInstanceOf[Seq[T]]) {
      def prev = prev0
      override val fromDelta = parentUpdated.fromDelta
      override val underlying = SeqDelta.patch(prev0.underlying, fromDelta)
    }
    case prev0: Sliced[_] => new Sliced[T](parentUpdated, prev0.from, prev0.until) {
      override lazy val fromDelta = Batch(SeqDelta flatten List(parentUpdated.fromDelta) flatMap {
        case Remove(i, e) if i >= from && i < until =>
          Remove(i - from, e) :: (if (prev0.parent.length > until)
            List(Include(until - 1 - from, prev0.parent.underlying(until)))
          else
            Nil
          )
        case Include(i, e) if i >= from && i < until =>
          List(Include(i - from, e))
        case Update(i, o, e) if i >= from && i < until =>
          List(Update(i - from, o, e))
        case _ => Nil
      }: _*)
      override lazy val underlying = SeqDelta.patch(prev0.underlying, fromDelta)
    }
    case prev0: PrefixWhile[_] => new PrefixWhile[T](parentUpdated, prev0.take, prev0.p.asInstanceOf[T => Boolean]) {
      override lazy val (fromDelta, valid) = {
        var vld = prev0.valid.asInstanceOf[Stream[(T, Boolean)]]
        def calcLastValid = vld.prefixLength({ case (_, b) => b })
        var lastValid = calcLastValid
        Batch(SeqDelta flatten List(parentUpdated.fromDelta) flatMap {
          case Include(i, e) =>
            val v = p(e)
            vld = vld.patch(i, List(e -> v), 0)
            if (i <= lastValid) {
              if (!v) lastValid = i - 1
              else lastValid += 1
            } else if (v && i == lastValid + 1) {
              lastValid = calcLastValid
            }
            filterIndex(lastValid, i) map (Include(_, e))
          case Remove(i, o) =>
            vld = vld.patch(i, Nil, 1)
            if (i <= lastValid) lastValid -= 1
            else if (i == lastValid + 1) lastValid = calcLastValid
            filterIndex(lastValid, i) map (Remove(_, o))
          case Update(i, o, e) =>
            val prev = vld(i)
            val v = p(e)
            vld = vld.patch(i, List(e -> v), 1)
            if (i <= lastValid && !v) lastValid = i - 1
            else if (i == lastValid + 1 && prev != v) lastValid = calcLastValid
            filterIndex(lastValid, i) map (Update(_, o, e))
        }: _*) -> vld
      }
    }
  }
}

trait DeltaSeq[+T] extends immutable.Seq[T] with GenericTraversableTemplate[T, DeltaSeq] with SeqLike[T, DeltaSeq[T]] {
  import DeltaSeq._

  override def companion = DeltaSeq

  val underlying: Seq[T]
  def fromDelta: SeqDelta[T, T]

  def apply(i: Int): T = underlying.apply(i)
  def length = underlying.length
  def iterator = underlying.iterator
  override def toString = "DeltaSeq("+underlying.toString+"("+fromDelta+"))"

  def ifDS[U, That](res: => DeltaSeq[U], sup: => That)(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]) =
    if (!bf.isInstanceOf[DeltaSeq.DeltaSeqCBF[T]]) sup else {
      val ret = res
      ret.asInstanceOf[That]
    }

  override def map[U, That](f: T => U)(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ifDS(new Folded[T, U](this, x => List(f(x))), super.map(f))
  override def flatMap[U, That](f: T => GTraversableOnce[U])(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ifDS(new Folded[T, U](this, x => f(x)), super.flatMap(f))
  override def filter(p: T => Boolean): Transformed[T, T] = new Folded[T, T](this, x => List(x) filter p)
  override def withFilter(p: T => Boolean) = filter(p)
  override def partition(p: T => Boolean) = (filter(p), filter(!p(_)))
  override def collect[U, That](pf: PartialFunction[T, U])(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ifDS(
      new Folded[T, U](this, x => if (pf.isDefinedAt(x)) List(pf(x)) else Nil),
      super.collect(pf)
    )
  override def slice(from: Int, until: Int): Transformed[T, T] =
    new Sliced[T](this, from, until)
  override def init = slice(0, underlying.size - 1)
  override def drop(n: Int) = slice(n max 0, underlying.size - n)
  override def take(n: Int) = slice(0, n)
  override def splitAt(n: Int) = (take(n), drop(n))
  override def dropWhile(p: T => Boolean): Transformed[T, T] =
    new PrefixWhile(this, false, p)
  override def takeWhile(p: T => Boolean): Transformed[T, T] =
    new PrefixWhile(this, true, p)
  override def ++[U >: T, That](xs: TraversableOnce[U])(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ifDS(
      new Appended[U](this, xs.toSeq),
      super.++(xs)
    )
  override def :+[U >: T, That](elem: U)(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ++(List(elem))(bf)
}
