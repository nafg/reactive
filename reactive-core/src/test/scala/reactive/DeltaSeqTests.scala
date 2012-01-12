package reactive
import java.io.{ PrintStream, ByteArrayOutputStream }

import scala.annotation.implicitNotFound

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ ParallelTestExecution, FunSuite }

class DeltaSeqTests extends FunSuite with ShouldMatchers with PropertyChecks with ParallelTestExecution {
  implicit val observing0 = new Observing {}
  val ts = DeltaSeq(1, 2, 3, 4)

  def xform(ds0: DeltaSeq[Int])(m: SeqDelta[Int, Int]) = ds0 match {
    case ds: DeltaSeq[Int]#Transformed[Int, Int] => ds.updatedFromParent(DeltaSeq.updatedByDeltas(ts, m)).fromDelta
  }

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
      Batch(Remove(0, 2), Include(1, 4), Remove(0, 3))
    )
  }
  test("++") {
    val appended = ts ++ Seq(5, 6)

    appended.underlying should equal (Seq(1, 2, 3, 4, 5, 6))

    xform(appended)(Remove(0, 1)) should equal (Remove(0, 1))
  }

  test("patch") {
    forAll(
      for (list <- arbitrary[List[Int]]; replacement <- arbitrary[List[Int]]; index <- Gen.choose(-1, list.length + 2); count <- arbitrary[Int]) yield (list, replacement, index, count),
      maxSize(20)
    ) {
        case (list, replacement, index, count) =>
          DeltaSeq(list: _*).patch(index, replacement, count) should equal (list.patch(index, replacement, count))
      }
  }
  test("check slice") {
    forAll(
      for (list <- arbitrary[List[Int]]; from <- Gen.choose(-1, list.length + 2); until <- Gen.choose(-1, list.length + 2)) yield (list, from, until),
      maxSize(20)
    ) {
        case (list, from, until) =>
          DeltaSeq(list: _*).slice(from, until) should equal (list.slice(from, until))
      }
  }

  def printStack[A](p: => A): A = {
    val baos = new ByteArrayOutputStream
    try {
      Console.withOut(new PrintStream(baos)) { p }
    } catch {
      case e =>
        Console.print(new String(baos.toByteArray()))
        println(Console.RED + e + Console.RESET)
        println
        throw e
    }
  }

  test("Repeating updatedFromParent") {
    def iter[T](s: BufferSignal[T], xss: List[List[T]])(f: List[T] => Unit) {
      printStack {
        println
        for (xs <- xss) {
          s () = xs
          withClue("s.now = "+s.now+":") { f(xs) }
        }
        println(" == Ok == ")
      }
    }
    println("==================")

    forAll(Gen.listOf1(arbitrary[List[(Int, List[Int])]]), maxSize(10)){
      case Nil => whenever(false) {}
      case head :: xss =>
        val s = BufferSignal(head: _*)
        val mapped = s.now.map(_._1 + 1).signal
        val flatMapped = s.now.flatMap(_._2).signal
        val filtered = s.now.filter(_._1 % 2 == 0).signal
        iter(s, xss) { xs =>
          mapped.now should equal (xs map (_._1 + 1))
          flatMapped.now should equal (xs flatMap (_._2))
          filtered.now should equal (xs filter (_._1 % 2 == 0))
        }
    }
    forAll(for (list <- Gen.listOf1(arbitrary[List[Int]]); from <- Gen.choose(-2, list.length + 2); to <- Gen.choose(-2, list.length + 2)) yield (list, from, to), maxSize(10)) {
      case (Nil, _, _) => whenever(false){}
      case (head :: xss, from, to) =>
        val s = BufferSignal(head: _*)
        val sliced = s.now.slice(from, to).signal
        iter(s, xss)(xs => sliced.now should equal (xs.slice(from, to)))
    }
    def checkPrefixBased: ((List[List[Int]], Int)) => Unit = {
      case (Nil, _) => whenever(false){}
      case (head :: xss, threshold) =>
        println("Values: "+(head :: xss).mkString(" :: ")+" :: Nil")
        println("Predicate: (_ < "+threshold+")")
        val s = BufferSignal(head: _*)
        val t = s.now.takeWhile(_ < threshold).signal
        val d = s.now.dropWhile(_ < threshold).signal
        iter(s, xss){ xs =>
          withClue("takeWhile: ")(t.now should equal (xs.takeWhile(_ < threshold)))
          withClue("dropWhile: ")(d.now should equal (xs.dropWhile(_ < threshold)))
        }
    }
    forAll(for (list <- Gen.listOf1(arbitrary[List[Int]]); threshold <- Gen.choose(0, 5)) yield (list, threshold), maxSize(10))(checkPrefixBased)
  }
}
