package reactive

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class SignalTests extends FunSuite with ShouldMatchers with CollectEvents with PropertyChecks {
  implicit val observing = new Observing {}
  test("map") {
    val s = Var(10)
    val mapped = s.map[Int, Signal[Int]](_ * 3)

    mapped.now should equal (30)

    collecting(mapped.change) {
      s () = 60
      mapped.now should equal (180)
    } should equal (List(180))
  }

  test("flatMap (regular T=>Signal[U])") {
    val aVar = Var(3)
    val vals = List(Val(1), Val(2), aVar)
    val parent = Var(0)
    val flatMapped = parent.flatMap(vals)

    collecting(flatMapped.change) {
      flatMapped.now should equal (1)

      parent () = 1
      flatMapped.now should equal (2)

      parent () = 2
      flatMapped.now should equal (3)
      aVar () = 4
      flatMapped.now should equal (4)

      parent () = 0
      flatMapped.now should equal (1)

    } should equal (List(2, 3, 4, 1))
  }

  ignore("flatMap (T=>SeqSignal[U])") {   //TODO is there any value in any part of this test after the new way of transforming SeqSignals?
    val bufSig1 = BufferSignal(1, 2, 3)
    val bufSig2 = BufferSignal(2, 3, 4)
    val parent = Var(false)
    val flatMapped = SeqSignal(parent.flatMap { b: Boolean =>
      if (!b) bufSig1 else bufSig2
    })

    flatMapped.now should equal (Seq(1, 2, 3))

    collecting(flatMapped.deltas){
      collecting(flatMapped.change){
        System.gc()
        parent () = true
        flatMapped.now should equal (Seq(2, 3, 4))
      } should equal (List(List(2, 3, 4)))
    }.toBatch.applyToSeq(List(1, 2, 3)) should equal (List(2, 3, 4))

    collecting(flatMapped.deltas){
      collecting(flatMapped.change){
        System.gc
        bufSig2 () = Seq(2, 3, 4, 5)
        flatMapped.now should equal (Seq(2, 3, 4, 5))
      } should equal (List(List(2, 3, 4, 5)))
    } should equal (List(
      Batch(Include(3, 5))
    ))

    collecting(flatMapped.deltas){
      collecting(flatMapped.change){
        parent () = false
        flatMapped.now should equal (Seq(1, 2, 3))
      } should equal (List(List(1, 2, 3)))
    }.toBatch.applyToSeq(List(2, 3, 4, 5)) should equal (List(1, 2, 3))
  }

  ignore("flatMap(value => seqSignal.map(_ filter pred))") {  //TODO is there any value in any part of this test after the new way of transforming SeqSignals?
    val switch = Var(false)
    val numbers = BufferSignal(1, 2, 3, 4, 5)
    val filteredNumbers = numbers.map(_ filter (_ < 3))
    val flatMapped = SeqSignal(switch.flatMap {
      case false => numbers
      case true  => filteredNumbers
    })

    SeqDelta.flatten(numbers.now.fromDelta :: Nil) should equal (List(
      Include(0, 1), Include(1, 2), Include(2, 3), Include(3, 4), Include(4, 5)
    ))
    filteredNumbers.now.fromDelta should equal (Batch(
      Include(0, 1), Include(1, 2)
    ))

    collecting(flatMapped.deltas) {
      switch () = true
    } should equal (List(
      Batch(
        Remove(2, 3), Remove(2, 4), Remove(2, 5)
      )
    ))
  }

  test("flatMap(value => EventStream)") {
    val s = Var(10)
    val es = new EventSource[Int]
    val fm = s.flatMap(x => es.map(x*))
    collecting(fm){ es fire 3 } should equal(List(30))
    collecting(fm){ s () = 20 } should equal (Nil)
    collecting(fm){ es fire 4 } should equal (List(80))
  }

  test("distinct") {
    val v = Var(2)
    val vd = v.distinct
    collecting(vd.change) {
      v () = 2
      v () = 4
      v () = 4
      v () = 2
    } should equal (List(4, 2))
  }

  test("nonblocking: no re-entry") {
    val v0 = Var(0)
    val v = Var(1)

    var n = 0
    for {
      _ <- v0
      _ <- v.nonblocking
    } {
      n += 1
      n should equal (1)
      n -= 1
    }

    for (a <- 1 to 10 toList) {
      scala.concurrent.ops.spawn {
        for (b <- 1 to 10)
          v () = v.now + 1
      }
    }
  }

  test("Signals should fire change event after 'now' is set") {
    def check[T, S[_] <: Signal[_]](sig: S[T]): S[T] = { sig.change =>> (_ should equal (sig.now)); sig }
    val v = check(Var(10))
    val mapped = check(v.map(_ + 10))
    val flatMapped = check(v.flatMap(x => Var(x + 15): Signal[Int]))
    val distinct = check(v.distinct)
    val nr = check(v.nonrecursive)
    val nb = check(v.nonblocking)
    v () = 20
  }

  test("sequence") {
    val bufSig = BufferSignal(Var(1), Var(2), Var(3), Var(4))

    val agg = bufSig.sequence map (_.sum)
    agg.now should equal (10)
    bufSig.value(2) () = 6
    agg.now should equal (13)
    bufSig.value += Var(20)
    agg.now should equal (33)
    bufSig.value remove 0
    agg.now should equal (32)

    // inserts and removes, i.e., containing signal change
    forAll(Arbitrary.arbitrary[List[Int]], maxSize(10)) { xs =>
      bufSig () = xs.map(Var(_))
      agg.now should equal (xs.sum)

      if (bufSig.value nonEmpty) {
        // updates to member signals
        forAll(Gen.choose(0, bufSig.value.length - 1), Arbitrary.arbitrary[Int]){
          case (i, x) =>
            bufSig.value(i) () = x
            agg.now should equal (bufSig.now map (_.now) sum)
        }
      }
    }

  }
}

class VarTests extends FunSuite with ShouldMatchers with CollectEvents with Observing {
  test("<-->") {
    val a = Var(10)
    val b = Var(20) <--> a
    a.now should equal (20)

    b () = 3
    a.now should equal (3)

    a () = 16
    b.now should equal (16)
  }
}

object Run {
  def main(args: Array[String]) = new SignalTests().execute()
}
