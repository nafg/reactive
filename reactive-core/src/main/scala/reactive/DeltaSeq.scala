package reactive

import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.SeqFactory
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.GenTraversableOnce
import scala.collection.mutable.Builder
import scala.collection.immutable
import scala.collection.SeqLike

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

  def startDelta[A](xs: Seq[A], off: Int = 0): SeqDelta[A, A] = Batch(xs.zipWithIndex map { case (v, i) => Include(i + off, v) }: _*)
  def fromSeq[A](xs: Seq[A]): DeltaSeq[A] = new DeltaSeq[A] {
    val underlying = xs
    val fromDelta = startDelta(xs)
    val signal: SeqSignal[A] = new Val[DeltaSeq[A]](this) with SeqSignal[A]
  }
  override def apply[A](xs: A*) = fromSeq(xs)

  def updatedByValue[T](prev: DeltaSeq[T], seq: Seq[T]): DeltaSeq[T] = new DeltaSeq[T] {
    val signal = prev.signal
    val underlying = seq
    val fromDelta = Batch(LCS.lcsdiff[T, T](prev.underlying, seq, _ == _): _*)
  }
  def updatedByDeltas[T](prev0: DeltaSeq[T], delta: SeqDelta[T, T]): DeltaSeq[T] = new DeltaSeq[T] {
    val signal = prev0.signal
    val fromDelta = delta
    val underlying = SeqDelta.patch(prev0.underlying, fromDelta)
  }
}

trait DeltaSeq[+T] extends immutable.Seq[T] with GenericTraversableTemplate[T, DeltaSeq] with SeqLike[T, DeltaSeq[T]] {
  import DeltaSeq._
  sealed abstract class Transformed[T, V] extends DeltaSeq[V] {
    type This <: Transformed[T, V]
    def parent: DeltaSeq[T]
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
    def updatedFromParent(parentUpdated: DeltaSeq[T]): This

    lazy val signal = new SeqSignal[V] {
      private var current: This = Transformed.this.asInstanceOf[This]
      current.underlying
      def now = current
      val pc: Event[DeltaSeq[T]] => Unit = _ foreach { ds =>
        current = current.updatedFromParent(ds.immutableCopy).asInstanceOf[This]
        current.underlying
        change fire now
      }
      private val _sub = parent.signal.change subscribe pc
      lazy val change = new EventSource[DeltaSeq[V]] {}
    }
  }
  class Appended[T](val parent: DeltaSeq[T], val xs: Seq[T]) extends Transformed[T, T] { prev =>
    type This = Appended[T]
    val (underlying, fromDelta) = {
      val pu = parent.underlying
      (pu ++ xs, startDelta(pu ++ xs))
    }
    def updatedFromParent(parentUpdated: DeltaSeq[T]): Appended[T] = new Appended[T](parentUpdated, prev.xs.asInstanceOf[Seq[T]]) {
      override val fromDelta = parentUpdated.fromDelta
      override val underlying = SeqDelta.patch(prev.underlying, fromDelta)
    }
  }
  class FlatMapped[T, V](val parent: DeltaSeq[T], val f: T => GenTraversableOnce[V]) extends Transformed[T, V] { prev =>
    type This = FlatMapped[T, V]
    lazy val (underlying, indexMap) = {
      val buf = new ArrayBuffer[V]
      var map = scala.collection.immutable.Map.empty[Int, Seq[Int]]
      var j = 0
      for ((x, i) <- parent.underlying.zipWithIndex) {
        val y = f(x)
        val yl = y.toList
        buf ++= yl
        map = map.updated(i, j to j + yl.size)
        j += y.size
      }
      (buf.toSeq, map)
    }
    lazy val fromDelta: SeqDelta[V, V] = startDelta(underlying)

    def updatedFromParent(parentUpdated: DeltaSeq[T]): FlatMapped[T, V] = new FlatMapped[T, V](parentUpdated, prev.f) {
      override lazy val (fromDelta, indexMap, underlying) = {
        var map = prev.indexMap
        val ds = new ArrayBuffer[SeqDelta[V, V]]
        val buf = prev.underlying.toBuffer
        def applyDelta(d: SingleDelta[T, T]): Unit = d match {
          case Remove(i, _) => // convert a remove on parent.prev to a remove on prev  (parent == parentUpdated)
            val prevFlatmappedIndices = map(i) dropRight 1
            map = map - i map {
              case (j, xs) if j > i => (j - 1, xs map (_ - prevFlatmappedIndices.size))
              case other            => other
            }
            // all removes happen at same index
            prevFlatmappedIndices foreach { _ =>
              ds += Remove(prevFlatmappedIndices.head, buf(prevFlatmappedIndices.head))
              buf.remove(prevFlatmappedIndices.head)
            }
          case Include(i, e) =>
            val startIndex = map get i map (_.head) getOrElse buf.length
            val res = f(e)
            val resSeq: List[V] = res.toList
            resSeq.zipWithIndex foreach {
              case (v, j) =>
                buf.insert(startIndex + j, v)
                ds += Include(startIndex + j, v)
            }
            map = map.map {
              case (j, xs) if j >= i => (j + 1, xs map (_ + resSeq.length))
              case other             => other
            } + (i -> (startIndex until startIndex + resSeq.length + 1))
          case Update(i, o, e) =>
            applyDelta(Remove(i, o))
            applyDelta(Include(i, e))
        }
        SeqDelta flatten List(parentUpdated.fromDelta) foreach { d =>
//          val beforeMsg = "Delta: "+d+"\n>buf: "+buf+"\n>map: "+map+"\n>ds: "+ds
//          try {
            applyDelta(d)
//          } finally {
//              println("<buf: "+buf)
//              println("<map: "+map)
//              println("<ds: "+ds)
//          }
        }
        (Batch(ds.toSeq: _*), map, buf.toSeq)
      }
    }
  }
  class Sliced[T](val parent: DeltaSeq[T], val from: Int, val until: Int) extends Transformed[T, T] { prev =>
    type This = Sliced[T]
    lazy val underlying = parent.underlying.slice(from, until).toList
    lazy val fromDelta = startDelta(underlying)
    /*
     * prev.parent: The DeltaSeq the original (previous) Sliced is a window of
     * parentUpdated: The result of applying deltas to prev.parent, considered the parent of the new Sliced
     * from: The index of the parent which is mapped to index 0 in the Sliced
     * until: The first index of the parent which is not mapped in the Sliced. This is allowed to be greater than
     *        parent.length so that future additions appear in the window, but calculations should use (until min parent.length).
     *        The Sliced should therefore always have length (until min parent.length) - from
     * 
     * Includes and Removes on prev.parent (parentUpdated.fromDelta) can occur either before the window, inside the window, or after the window.
     * 
     * Those that occur after the window do not directly affect the Sliced.
     * After the window means i >= until.
     * 
     * Those that occur before the window affect the Sliced as if they occurred at '''from''', with the element at that location.
     * Before the window means i < from.
     * For instance, List(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14).slice(5,10) == List(5,6,7,8,9).
     * If an extra zero was inserted at the beginning of the original List, the slice would become List(4,5,6,7,8) --- as though a 4 had been inserted.
     * Conversely, if the zero was deleted, the slice would become (6,7,8,9,10) --- as though the 5 had been deleted.
     * So an insert before the window is as if the item at (from - 1) was inserted at from, and a remove before the window is as if the item at from was deleted.
     * 
     * Inserts within the window affect the slice as follows:
     *  1. The element is inserted at the translated (i - from) index.
     *  2. If the slice would now be longer than (until - from), the last element is removed.
     * Removes within the window affect the slice as follows:
     *  1. The element at the translated index is removed.
     *  2. If the slice would now be shorter than (until - from) and does not already include the last element of the parent (that is,
     *     slice.length + from < parent.length), insert the next element of the parent (parent(slice.length + from)).
     */
    def updatedFromParent(parentUpdated: DeltaSeq[T]): Sliced[T] = new Sliced[T](parentUpdated, prev.from, prev.until) {
      override lazy val (fromDelta, underlying) = {
        val ds = new ArrayBuffer[SeqDelta[T, T]]
        val buf = prev.toList.toBuffer
        var parentBuf = prev.parent.toList.toBuffer
        def actUntil = until min parentBuf.length
        def applyDelta(d: SingleDelta[T, T]): Unit = d match {
          case Remove(i, e) =>
            if ((i max from) < actUntil) {
              val j = i - from max 0
              ds += Remove(j, buf(j))
              buf.remove(j)
            }
            parentBuf.remove(i)
            if (buf.length < until - from && buf.length + from < parentBuf.length) {
              ds += Include(buf.length, parentBuf(buf.length + from))
              buf.insert(buf.length, parentBuf(buf.length + from))
            }
          case Include(i, e) =>
            parentBuf.insert(i, e)
            if (i < actUntil && parentBuf.length > (i max from)) {
              val (j, v) = if (i >= from) (i - from, e) else (0, parentBuf(from))
              ds += Include(j, v)
              buf.insert(j, v)
              if (buf.length > until - from) {
                ds += Remove(buf.length - 1, buf(buf.length - 1))
                buf.remove(buf.length - 1)
              }
            }
          case Update(i, o, e) =>
            applyDelta(Remove(i, o))
            applyDelta(Include(i, e))
        }
        //        println("prev.parent: "+prev.parent)
        //        println("parentUpdated: "+parentUpdated)
        //        println("(from, until): "+(from, until))
        SeqDelta flatten List(parentUpdated.fromDelta) foreach { d =>
          //          println("Delta: "+d)
          //          println(">ds: "+ds)
          //          println(">buf: "+buf)
          //          println(">parentBuf: "+parentBuf)
          applyDelta(d)
          //          println("<ds: "+ds)
          //          println("<buf: "+buf)
          //          println("<parentBuf: "+parentBuf)
        }
        (Batch(ds: _*), buf.toList)
      }
    }
  }
  class TakenWhile[T](val parent: DeltaSeq[T], val p: T => Boolean) extends Transformed[T, T] { prev =>
    type This = TakenWhile[T]
    lazy val predValues = parent.underlying.toStream map { x => x -> p(x) }
    lazy val underlying: Seq[T] = predValues.takeWhile{ case (_, b) => b }.map(_._1)
    lazy val fromDelta = startDelta(underlying)

    /*
     * takeWhile and dropWhile both see a sequence as divided into a prefix of elements that pass a predicate, and the remaining elements,
     * beginning with the first element that fails the predicate. (It makes no difference whether subsequent elements pass it or fail it.)
     * The difference is that takeWhile returns the prefix, and dropWhile returns the remaining elements. Here we discuss takeWhile.
     * 
     * Edits can occur within the prefix specified by the predicate, at its border, or outside it.
     * At its border means either removing the first element failing the predicate, inserting a new element at its index, or updating the value at its index.
     * 
     * An edit occurring at the first element for which the predicate fails can cause any number of elements to be included,
     * if the element that will now follow the current prefix will pass the predicate.
     * 
     * An edit more than one past the end of the prefix has no effect.
     * 
     * A remove, or insert or update whose new element passes the predicate, that occurs before the end of the prefix, can be applied directly.
     * 
     * An insert or update within the prefix, whose new element fails the predicate, is not applied,
     * and subsequent elements are removed, as the prefix has been shortened.
     * 
     * prefixLength: The number of elements in a row at the beginning of the parent sequence that pass the predicate. Consequently, the first index
     * that is either outside of the parent sequence or whose element fails the predicate.
     * 
     */
    def updatedFromParent(parentUpdated: DeltaSeq[T]): TakenWhile[T] = new TakenWhile[T](parentUpdated, prev.p.asInstanceOf[T => Boolean]) {
      override lazy val (fromDelta, predValues, underlying) = {
        val buf = prev.toList.toBuffer
        val ds = new ArrayBuffer[SeqDelta[T, T]]
        var prdVals = prev.predValues
        def calcPrefixLength = {
          def loop(n: Int, s: Stream[(T, Boolean)]): Int = if (s.isEmpty || !s.head._2) n else loop(n + 1, s.tail)
          loop(0, prdVals)
        }
        var prefixLength = calcPrefixLength
        def applyDelta(d: SingleDelta[T, T]): Unit = {
          val oldPrefixLength = prefixLength
          d match {
            case Include(i, e) =>
              val v = p(e)
              prdVals = prdVals.patch(i, List(e -> v), 0)

              if (i < prefixLength) {
                if (v) prefixLength += 1 else prefixLength = i
              } else if (v && i == prefixLength) {
                prefixLength = calcPrefixLength
              }

              if (v && i <= prefixLength && i <= buf.length) {
                ds += Include(i, e)
                buf.insert(i, e)
              }

              if (prefixLength > oldPrefixLength) for (j <- oldPrefixLength + 1 until prefixLength) {
                if (j <= buf.length) {
                  ds += Include(j, prdVals(j)._1)
                  buf.insert(j, prdVals(j)._1)
                }
              }
              else if (prefixLength < oldPrefixLength) for (j <- prefixLength until oldPrefixLength) {
//                println("j: "+j)
                if (prefixLength < buf.length) {
                  ds += Remove(prefixLength, prdVals(j)._1)
                  buf.remove(prefixLength)
                }
              }

            case Remove(i, o) =>
              prdVals = prdVals.patch(i, Nil, 1)
              if (i < prefixLength)
                prefixLength -= 1
              else if (i == prefixLength)
                prefixLength = calcPrefixLength

              if (i <= prefixLength && i < buf.length) {
                ds += Remove(i, o)
                buf.remove(i)
              }

              if (prefixLength > oldPrefixLength) for (j <- oldPrefixLength until prefixLength) {
                if (j <= buf.length && (prdVals isDefinedAt j)) {
                  ds += Include(j, prdVals(j)._1)
                  buf.insert(j, prdVals(j)._1)
                }
              }
              else if (prefixLength < oldPrefixLength) {
                for (j <- prefixLength until oldPrefixLength) {
//                  println("j: "+j)
                  if (j < buf.length && (prdVals isDefinedAt j)) {
                    ds += Remove(prefixLength, prdVals(j)._1)
                    buf.remove(prefixLength)
                  }
                }
              }

            case Update(i, o, e) =>
              applyDelta(Remove(i, o))
              applyDelta(Include(i, o))
          }
        }
        //        println("prev.parent: "+prev.parent)
        //        println("parentUpdated: "+parentUpdated)
        //        println("take: "+take)
        SeqDelta flatten List(parentUpdated.fromDelta) foreach { d =>
          //          println("Delta: "+d)
          //          println(">prdVals: "+prdVals)
          //          println(">prefixLength: "+prefixLength)
          //          println(">buf: "+buf)
          //          println(">ds: "+ds)
          applyDelta(d)
          //          println("<prdVals: "+prdVals)
          //          println("<prefixLength: "+prefixLength)
          //          println("<buf: "+buf)
          //          println("<ds: "+ds)
        }
        (Batch(ds: _*), prdVals, buf.toSeq)
      }
    }
  }
  class DroppedWhile[T](val parent: DeltaSeq[T], val p: T => Boolean) extends Transformed[T, T] { prev =>
    type This = DroppedWhile[T]
    lazy val predValues: Stream[(T, Boolean)] = parent.underlying.toStream map { x => x -> p(x) }
    lazy val underlying: Seq[T] = predValues.dropWhile{ case (_, b) => b }.map(_._1)
    lazy val fromDelta = startDelta(underlying)

    /*
     * takeWhile and dropWhile both see a sequence as divided into a prefix of elements that pass a predicate, and the remaining elements,
     * beginning with the first element that fails the predicate. (It makes no difference whether subsequent elements pass it or fail it.)
     * The difference is that takeWhile returns the prefix, and dropWhile returns the remaining elements. Here we discuss dropWhile.
     * 
     * Edits can occur within the prefix specified by the predicate, at its border, or outside it.
     * At its border means either removing the first element failing the predicate, inserting a new element at its index, or updating the value at its index.
     * 
     * An edit occurring at the first element for which the predicate fails can cause any number of elements to be removed, if the element that will now follow the
     * prefix will pass the predicate.
     * 
     * An edit more than one past the end of the prefix is applied after translating it (i - prefixLength).
     * 
     * A remove, or insert or update whose new element passes the predicate, that occurs before the end of the prefix, has no effect.
     * 
     * An insert or update within the prefix, whose new element fails the predicate, causes it and subsequent elements to be inserted;
     * since the prefix has been shortened, the set of remaining elements has expanded.
     * 
     * prefixLength: The number of elements in a row at the beginning of the parent sequence that pass the predicate. Consequently, the first index
     * that is either outside of the parent sequence or whose element fails the predicate.
     * 
     */
    def updatedFromParent(parentUpdated: DeltaSeq[T]): DroppedWhile[T] = new DroppedWhile[T](parentUpdated, prev.p.asInstanceOf[T => Boolean]) {
      override lazy val (fromDelta, predValues, underlying) = {
        val buf = prev.toList.toBuffer
        val ds = new ArrayBuffer[SeqDelta[T, T]]
        var prdVals: Stream[(T, Boolean)] = prev.predValues
        def calcPrefixLength = {
          def loop(n: Int, s: Stream[(T, Boolean)]): Int = if (s.isEmpty || !s.head._2) n else loop(n + 1, s.tail)
          loop(0, prdVals)
        }
        var prefixLength = calcPrefixLength
        def applyDelta(d: SingleDelta[T, T]): Unit = {
          val oldPrefixLength = prefixLength
          val oldPrdVals = prdVals
          d match {
            case Include(i, e) =>
              val v = p(e)
              prdVals = prdVals.patch(i, List(e -> v), 0)

              /*
               * Efficiently adjust the prefix length.
               * If the parent insertion occurred within the prefix and it passes the predicate, the prefix grew by one.
               * If it occurred within the prefix and it fails the predicate, the prefix now ends there.
               * If it occurred immediately after the prefix and it passes the predicate, we have to recalculate the prefix length.
               * If it occurred immediately after the prefix and it fails the predicate, or if it occurred further, the prefix length has not changed.
               */
              if (i < prefixLength) {
                if (v) prefixLength += 1 else prefixLength = i
              } else if (i == prefixLength) {
                if (v) prefixLength = calcPrefixLength
              }

              /*
               * If appropriate, apply the insertion.
               * Specifically, the criteria is that the insertion must have occurred outside of the adjusted prefix.
               * For instance, if an element that passes the predicate was inserted immediately following the prefix,
               * causing the prefix to grow, we do not insert anything, since dropping-while means dropping the prefix.
               * On the other hand if an element that fails the predicate was inserted within the prefix,
               * causing it to shrink, we insert it, since it's outside of the adjusted prefix.
               * Before applying the insertion we translate it by subtracting prefixLength from it.
               */
              val adjust = if (i >= prefixLength) {
                val j = i - prefixLength min buf.length
//                println("Applying original Include at "+j)
                ds += Include(j, e)
                buf.insert(j, e)
                1
              } else 1

              /*
               * If the prefix length has changed, we have more work to do.
               * If the prefix has grown there may be elements to remove.
               */
              if (prefixLength > oldPrefixLength + adjust) for (j <- oldPrefixLength + adjust until prefixLength) {
                if (j < buf.length) {
                  ds += Remove(0, prdVals(oldPrefixLength)._1)
                  buf.remove(0)
                }
              }
              else if (prefixLength < oldPrefixLength) for (j <- prefixLength until oldPrefixLength) {
//                println("j: "+j)
                if (j - prefixLength <= buf.length) {
                  ds += Include(j - prefixLength + 1, prdVals(j + 1)._1)
                  buf.insert(j - prefixLength + 1, prdVals(j + 1)._1)
                }
              }

            case Remove(i, o) =>
              prdVals = prdVals.patch(i, Nil, 1)

              /*
               * If an element was removed within the prefix, it automatically shrinks the prefix by one.
               * If the element immediately following the prefix was removed, the prefix needs to be recalculated.
               * If a further element was removed, it doesn't affect the prefix.
               */
              if (i < prefixLength)
                prefixLength -= 1
              else if (i == prefixLength)
                prefixLength = calcPrefixLength

              val adjust = if (i >= prefixLength) {
                val j = i - oldPrefixLength //max 0
                if (j < buf.length & j >= 0) {
//                  println("Applying original Remove at "+j)
                  ds += Remove(j, o)
                  buf.remove(j)
                  1
                } else 0
              } else 0

              if (prefixLength > oldPrefixLength) for (j <- oldPrefixLength until prefixLength + 1 - adjust) {
                if (buf.nonEmpty && (oldPrdVals isDefinedAt j)) {
//                  println("Applying additional Remove at "+j)
                  ds += Remove(0, oldPrdVals(j)._1)
                  buf.remove(0)
                }
              }
            //              else if (prefixLength < oldPrefixLength) for (j <- prefixLength until oldPrefixLength) {
            //                println("j: "+j)
            //                if (i >= prefixLength && j - prefixLength <= buf.length && (prdVals isDefinedAt j + 1)) {
            //                  ds += Include(j - prefixLength, prdVals(j + 1)._1)
            //                  buf.insert(j - prefixLength, prdVals(j + 1)._1)
            //                }
            //              }

            case Update(i, o, e) =>
              applyDelta(Remove(i, o))
              applyDelta(Include(i, o))
          }
        }
//        println("dropWhile")
//        println("prev.parent: "+prev.parent)
//        println("parentUpdated: "+parentUpdated)
        SeqDelta flatten List(parentUpdated.fromDelta) foreach { d =>
//          println("Delta: "+d)
//          println(">prdVals: "+prdVals)
//          println(">prefixLength: "+prefixLength)
//          println(">buf: "+buf)
//          println(">ds: "+ds)
          applyDelta(d)
//          println("<prdVals: "+prdVals)
//          println("<prefixLength: "+prefixLength)
//          println("<buf: "+buf)
//          println("<ds: "+ds)
        }
        (Batch(ds: _*), prdVals, buf.toSeq)
      }
    }
  }

  override def companion = DeltaSeq

  def signal: SeqSignal[T]

  val underlying: Seq[T]
  def fromDelta: SeqDelta[T, T]

  def apply(i: Int): T = underlying.apply(i)
  def length = underlying.length
  def iterator = underlying.iterator
  override def toString = "DeltaSeq("+underlying.toString+"("+fromDelta+"))"

  private def immutableCopy = underlying match {
    case _: scala.collection.immutable.Seq[_] =>
      this
    case xs =>
      new DeltaSeq[T] {
        val underlying = xs.toList
        def fromDelta = DeltaSeq.this.fromDelta
        def signal = DeltaSeq.this.signal
      }
  }

  def ifDS[U, That](res: => DeltaSeq[U], sup: => That)(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]) =
    if (!bf.isInstanceOf[DeltaSeq.DeltaSeqCBF[T]]) sup else {
      val ret = res
      ret.asInstanceOf[That]
    }

  override def map[U, That](f: T => U)(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ifDS(new FlatMapped[T, U](immutableCopy, x => List(f(x))), super.map(f))
  override def flatMap[U, That](f: T => GenTraversableOnce[U])(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ifDS(new FlatMapped[T, U](immutableCopy, x => f(x)), super.flatMap(f))
  override def filter(p: T => Boolean): DeltaSeq[T] = new FlatMapped[T, T](immutableCopy, x => List(x) filter p)
  override def withFilter(p: T => Boolean) = filter(p)
  override def partition(p: T => Boolean) = (filter(p), filter(!p(_)))
  override def collect[U, That](pf: PartialFunction[T, U])(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ifDS(
      new FlatMapped[T, U](immutableCopy, x => if (pf.isDefinedAt(x)) List(pf(x)) else Nil),
      super.collect(pf)
    )
  override def slice(from: Int, until: Int): DeltaSeq[T] =
    new Sliced[T](immutableCopy, from max 0, until)
  override def init = slice(0, underlying.size - 1)
  override def drop(n: Int) = slice(n max 0, underlying.size + 1)
  override def take(n: Int) = slice(0, n)
  override def splitAt(n: Int) = (take(n), drop(n))
  override def takeWhile(p: T => Boolean): DeltaSeq[T] = new TakenWhile(immutableCopy, p)
  override def dropWhile(p: T => Boolean): DeltaSeq[T] = new DroppedWhile(immutableCopy, p)
  override def ++[B >: T, That](xs: GenTraversableOnce[B])(implicit bf: CanBuildFrom[DeltaSeq[T], B, That]): That =
    ifDS(
      new Appended[B](immutableCopy, xs.toList),
      super.++(xs)
    )
  override def :+[U >: T, That](elem: U)(implicit bf: CanBuildFrom[DeltaSeq[T], U, That]): That =
    ++(List(elem))(bf)
}
