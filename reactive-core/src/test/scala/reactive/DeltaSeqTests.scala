package reactive
import org.scalatest.FunSuite

import org.scalatest.matchers.ShouldMatchers

class DeltaSeqTests extends FunSuite with ShouldMatchers {
  implicit val observing0 = new Observing {}
  val ts = DeltaSeq(1, 2, 3, 4)

  def xform(ds: DeltaSeq[Int])(m: SeqDelta[Int, Int]) = DeltaSeq.updatedFromParent(ds.asInstanceOf[DeltaSeq.Transformed[Int, Int]], DeltaSeq.updatedByDeltas(ts, m)).fromDelta

  test("(untransformed)") {
    ts.fromDelta should equal (Batch(
      Include(0, 1), Include(1, 2), Include(2, 3), Include(3, 4)
    ))
  }
  test("map") {
    val mapped = ts.map(_ + 1)

    mapped.underlying should equal (Seq(2, 3, 4, 5))

    xform(mapped)(Batch(Include(1, 2), Include(1, 3))) should equal (
      Batch(Include(1, 3), Include(1, 4))
    )
  }

  test("flatMap") {
    val flatMapped = ts.flatMap { i => Seq(i, i * 10) }

    flatMapped.underlying should equal (Seq(1, 10, 2, 20, 3, 30, 4, 40))

    xform(flatMapped)(Batch(Include(1, 3), Include(1, 4))) should equal (
      Batch(Include(2, 3), Include(3, 30), Include(2, 4), Include(3, 40))
    )
  }

  test("filter") {
    val filtered = ts.filter { _ % 2 == 0 }

    filtered.underlying should equal (Seq(2, 4))

    xform(filtered)(Remove(3, 4)) should equal (Batch(Remove(1, 4)))
    xform(filtered)(Remove(0, 1)) should equal (Batch())
  }

  test("takeWhile") {
    val takenWhile = ts.takeWhile(_ < 3)

    takenWhile.underlying should equal (Seq(1, 2))

    xform(takenWhile)(Batch(Remove(1, 2), Remove(1, 3))) should equal (
      Batch(Remove(1, 2))
    )
  }
  test("dropWhile") {
    val droppedWhile = ts.dropWhile(_ < 3)

    droppedWhile.underlying should equal (Seq(3, 4))

    xform(droppedWhile)(Batch(Remove(1, 2), Remove(1, 3))) should equal (
      Batch(Remove(0, 3))
    )
  }
  test("slice") {
    val sliced = ts.slice(1, 3)

    sliced.underlying should equal (Seq(2, 3))

    xform(sliced)(Batch(Remove(1, 2), Remove(1, 3))) should equal (
      Batch(Remove(0, 2), Include(1, 4), Remove(0, 3), Include(1, 4))
    )
  }
  test("++") {
    val appended = ts ++ Seq(5, 6)

    appended.underlying should equal (Seq(1, 2, 3, 4, 5, 6))

    xform(appended)(Remove(0, 1)) should equal (Remove(0, 1))
  }
}