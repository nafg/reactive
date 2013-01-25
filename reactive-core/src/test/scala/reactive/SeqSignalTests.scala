package reactive
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.util.Random

import CollectEvents._

class ObservableBufferTests extends FunSuite with ShouldMatchers with Observing {
  test("consistency between mutations and applyDeltas") {
    val ob1 = new ObservableBuffer[Int]
    val ob2 = new ObservableBuffer[Int]
    ob1.messages foreach { delta =>
      //      println("ob1: " + ob1)
      //      println("ob2: " + ob2)
      //      println("applying: " + delta)
      ob2.applyDelta(delta)
    }
    for (_ <- 1 to 100) {
      Random.nextInt(6) match {
        case 0 => ob1 += Random.nextInt
        case 1 => Random.nextInt +=: ob1
        case 2 => if (ob1.nonEmpty) ob1.insertAll(Random.nextInt(ob1.length), (0 to Random.nextInt(100)) map (_ => Random.nextInt))
        case 3 => if (ob1.nonEmpty) ob1.update(Random.nextInt(ob1.length), Random.nextInt)
        case 4 => if (ob1.nonEmpty) ob1.remove(Random.nextInt(ob1.length))
        case 5 => if (Random.nextBoolean) ob1.clear
      }
      ob1 should equal(ob2)
    }
  }
}

class SeqSignalTests extends FunSuite with ShouldMatchers with Observing {
  test("map(Seq=>TransformedSeq)") {
    val ss = BufferSignal(1, 2, 3)
    val mapped = ss.now.map(_ * 10).signal

    implicitly[mapped.type <:< Signal[DeltaSeq[Int]]]
    implicitly[mapped.type <:< SeqSignal[Int]]
    collecting(mapped.deltas) {
      ss.value += 4
    } should equal(List(Batch(
      Include(3, 40)
    )))
    mapped.now should equal (List(10, 20, 30, 40))
    val flatMapped = ss.now.flatMap{ n =>
      <xml>{ n }</xml>
    }.signal
    collecting(mapped.deltas) {
      collecting(flatMapped.deltas) {
        ss.value += 5
      } should equal(List(Batch(
        Include(4, <xml>{ 5 }</xml>)
      )))
    } should equal(List(Batch(
      Include(4, 50)
    )))
    mapped.now should equal (List(10, 20, 30, 40, 50))
  }

  test("Factory (diff signal)") {
    val signal = Var(List(1, 2, 3))
    val diff = SeqSignal(signal)
    collecting(diff.deltas) {
      collecting(diff.change) {
        signal() = List(2, 3, 4)
      } should equal(List(
        Seq(2, 3, 4)
      ))
    } should equal(List(Batch(
      Remove(0, 1), Include(2, 4)
    )))
  }

  test("SeqSignal should fire deltas after now has the new value") {
    def test[T](s: SeqSignal[T]) = s.deltas.foreach { ds =>
      val flattened = SeqDelta.flatten(List(ds))
      flattened foreach {
        case d @ Include(i, n) =>
          s.now(i) should equal(n)
        case d =>
      }
    }

    val v = Var(List(1, 2, 3))
    val s = SeqSignal(v)
    test(s)
    v() = List(2, 3, 4)

    val s2 = s.now.map(_ * 10).signal
    test(s2)
    v() = List(3, 4, 5)

    val v2 = Var(10)
    val m = SeqSignal(v2.map(n => List(n, n + 1, n + 2)))
    test(m)
    v2 () = 20
  }

  test("flatMap-map-slice") {
    val xs = BufferSignal[Int]()
    for (i <- 1 until 10) xs.value += i

    val perPage = 2
    val page = Var(0)
    val curPageXs = SeqSignal(page.flatMap{ p =>
      //xs.map(_.drop(p * perPage)).map(_.take(perPage))
      xs.map(_.slice(p * perPage, p * perPage + perPage))
    })

    curPageXs.now should equal (Seq(1, 2))

    collecting(curPageXs.deltas){
      page () = 1
      curPageXs.now should equal(Seq(3, 4))
    } should equal (List(Batch(
      Remove(0, 1), Remove(0, 2), Include(0, 3), Include(1, 4)
    )))
    collecting(curPageXs.deltas){
      page () = 2
      curPageXs.now should equal(Seq(5, 6))
    } should equal (List(Batch(
      Remove(0, 3), Remove(0, 4), Include(0, 5), Include(1, 6)
    )))
  }

  test("SeqSignals should fire change event and delta event after 'now' is set") {
    def check[T, S[_] <: SeqSignal[_]](sig: S[T]): S[T] = {
      var before = true
      sig.change =>> { x => x should equal (sig.now); before = false }
      sig.deltas ->> { before should equal (false) }
      sig
    }
    val v = check(BufferSignal(10, 20))
    val mapped = check((v.now :+ 30).signal)
    val flatMapped = check(SeqSignal(v.flatMap(x => BufferSignal(40, 50))))
    v () = List(60, 70)
  }

  test("fromDeltas") {
    println("begin")
    val es = new EventSource[SeqDelta[Int, Int]]
    val sig = SeqSignal.fromDeltas(1 :: 2 :: 3 :: Nil, es)
    val sig2 = sig.now.map(_.toString).signal
    collecting(sig2.change) {
      es fire Include(1, 10)
    }.map(_.toList) should equal (List(
      List("1", "10", "2", "3")
    ))
    println("end")
  }
}

class BufferSignalTests extends FunSuite with ShouldMatchers with Observing {
  test("consistency") {
    val signal = BufferSignal(1, 2, 3)
    val ob = new ObservableBuffer[Int]
    ob += (1, 2, 3)
    var changes = 0
    var deltas = 0
    //    signal.deltas foreach {d => println("delta: " + d)}
    signal.deltas foreach { d =>
      ob.applyDelta(d)
      signal.now should equal(ob)
      deltas += 1
    }
    //    signal.change foreach {c => println("change: " + c)}
    //    signal.change foreach {_ should equal (ob)}
    signal.change foreach { _ => changes += 1 }

    withClue("Adding 4:"){ signal.value += 4 }
    signal.value -= 3
    signal() = Seq(10, 9, 8, 7, 6, 5, 4, 3)

    signal.now should equal(ob)
    changes should equal(3)
    deltas should equal(3)
  }
}

