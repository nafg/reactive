package reactive
import org.scalatest.FunSuite

import org.scalatest.matchers.ShouldMatchers

class TransformedSeqTests extends FunSuite with ShouldMatchers {
  implicit val observing0 = new Observing {}
  val ts = new TransformedSeq[Int] {
    val observing = observing0
    val underlying = List(1, 2, 3, 4)
  }

  def xform(t: TransformedSeq[Int])(m: Message[Int,Int]) = t match { case t: TransformedSeq[Int]#Transformed[Int] => t.xform(m)}

  test("map") {
    val mapped = ts.map(_ + 1)

    mapped.underlying should equal (Seq(2, 3, 4, 5))
    mapped.baseDeltas should equal (Seq(Remove(0, 1), Include(3, 5)))
    
    xform(mapped)(
      Batch(Include(1,2), Include(1,3))
    ) should equal (List(
      Batch(Include(1,3), Include(1,4))
    ))
  }

  test("flatMap") {
    val flatMapped = ts.flatMap { i => Seq(i, i * 10) }

    flatMapped.underlying should equal (Seq(1, 10, 2, 20, 3, 30, 4, 40))
    flatMapped.baseDeltas should equal (Seq(
      Include(1, 10),
      Include(3, 20),
      Include(5, 30),
      Include(7, 40)
      ))
    
    xform(flatMapped)(
      Batch(Include(1,3), Include(1,4))
    ) should equal (List(
      Batch(Include(2,3), Include(3, 30), Include(2,4), Include(3,40))
    ))
  }
  
  test("filter") {
    val filtered = ts.filter {_ % 2 == 0}
    
    filtered.underlying should equal (Seq(2,4))
    filtered.baseDeltas should equal (Seq(Remove(0,1), Remove(1,3)))
    
    xform(filtered)(Remove(3,4)) should equal (List(Remove(1,4)))
    xform(filtered)(Remove(0,1)) should equal (List())
  }
  
  test("takeWhile") {
    val takenWhile = ts.takeWhile(_ < 3)
    
    takenWhile.underlying should equal (Seq(1,2))
    takenWhile.baseDeltas should equal (Seq(Remove(2,3), Remove(2,4)))
    
    xform(takenWhile)(
      Batch(Remove(1,2),Remove(1,3))
    ) should equal (List(
      Batch(Remove(1,2))
    ))
  }
  test("dropWhile") {
    val droppedWhile = ts.dropWhile(_ < 3)
    
    droppedWhile.underlying should equal (Seq(3,4))
    droppedWhile.baseDeltas should equal (Seq(Remove(0,1), Remove(0,2)))
    
    xform(droppedWhile)(
      Batch(Remove(1,2),Remove(1,3))
    ) should equal (List(
      Batch(Remove(0,3))
    ))
  }
  test("slice") {
    val sliced = ts.slice(1,3)
    
    sliced.underlying should equal (Seq(2,3))
    sliced.baseDeltas should equal (Seq(Remove(0,1), Remove(2,4)))
    
    xform(sliced)(
      Batch(Remove(1,2),Remove(1,3))
    ) should equal (List(
      Batch(Remove(0,2), Remove(0,3))
    ))
  }
  test("++") {
    val appended = ts ++ Seq(5,6)
    
    appended.underlying should equal (Seq(1,2,3,4,5,6))
    appended.baseDeltas should equal (Seq(Include(4,5),Include(5,6)))
    
    xform(appended)(Remove(0,1)) should equal (List(Remove(0,1)))
  }
}