package reactive
import java.io.{ PrintStream, ByteArrayOutputStream }

import scala.annotation.implicitNotFound

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ ParallelTestExecution, FunSuite }

class DeltaSeqTests extends FunSuite with Matchers with PropertyChecks with ParallelTestExecution {
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
      case e: Exception =>
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
//          println("Changing s from "+s.now+" to "+xs)
          s () = xs
          withClue("s.now = "+s.now+":") { f(xs) }
        }
        println(" == Ok == ")
      }
    }
//    println("==================")
    //checkPrefixBased(List(List(true, false), List(false)))

    forAll(Gen.listOf1(arbitrary[List[(Int, List[Int])]]), maxSize(10)){
      case Nil => whenever(false) {}
      case head :: xss =>
        val s = BufferSignal(head: _*)
        val mapped = s.now.map(_._1 + 1).signal
        val flatMapped = s.now.flatMap(_._2).signal
//        s.change foreach println
//        s.deltas foreach println
//        flatMapped.change foreach println
//        flatMapped.deltas foreach println
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
    forAll(for (list1 <- Gen.listOf1(Gen.listOf1(arbitrary[Int])); list2 <- arbitrary[List[Int]]) yield (list1, list2), maxSize(10)) {
      case (head :: xss, toAppend) =>
        val s = BufferSignal(head: _*)
        val appended = (s.now ++ toAppend).signal
        iter(s, xss)(xs => appended.now should equal (xs ++ toAppend))
      case _ => whenever(false) {}
    }
    def checkPrefixBased: (List[List[Boolean]]) => Unit = {
      case Nil => whenever(false) {}
      case head :: xss =>
//        println("Values: "+(head :: xss).mkString(" :: ")+" :: Nil")
        val s = BufferSignal(head: _*)
        val t = s.now.takeWhile(_ == true).signal
        val d = s.now.dropWhile(_ == true).signal
        iter(s, xss){ xs =>
          withClue("dropWhile: ")(d.now should equal (xs.dropWhile(_ == true)))
          withClue("takeWhile: ")(t.now should equal (xs.takeWhile(_ == true)))
        }
    }
    val gen = Gen.listOf1(Gen.listOf1(arbitrary[Boolean]))
    forAll(gen, maxSize(10))(checkPrefixBased)
    /*
     // The REALLY thorough test -- systematic not random
     // currently only single removes
     val excs = for {
      i <- 1 to 3
      l1 <- (List.fill(i)(true) ++ List.fill(i)(false)).permutations
      j <- 0 to l1.length
      b <- List(true, false)
      l2 = l1.patch(j, List(b), 0)
      g = List(l1, l2)
      e <- try {
        checkPrefixBased(g)
        None
      } catch {
        case e => Some(g -> e)
      }
    } yield e
    println(excs.length+" failures")
    excs foreach {
      case (g, e) =>
        val line = e.getStackTrace().find(_.getFileName endsWith "DeltaSeq.scala")
        val msg = e.toString + line.map(" at "+_).getOrElse("")
    }
    excs.headOption foreach { case (_, e) => throw e }*/
  }
}
