package reactive

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.ParallelTestExecution

import reactive.logging._

object CollectEvents extends CollectEvents
//TODO add collecting method to EventStream
trait CollectEvents {
  private val observing1 = new Observing {}

  def collectingReturning[T, R](es: EventStream[T])(f: =>R): (List[T], R) = {
    var log = List[T]()
    var executing = true
    es
      .takeWhile(_ => executing)
      .foreach(log :+= _)(observing1)
    val ret = f
    executing = false
    (log, ret)
  }

  def collecting[A](es: EventStream[A])(f: =>Unit): List[A] = collectingReturning[A, Unit](es)(f)._1
}

class LoggerTests extends FunSuite with Matchers with Observing {
  test("doesn't StackOverflow") {
    Logger.defaultLevel = LogLevel.Trace
    Logger.all foreach { _ => }
    val es = new EventSource[Int]
    es foreach { _ => }
    es fire 1
  }
}

class EventStreamTests extends FunSuite with Matchers with CollectEvents with ParallelTestExecution {
  implicit val observing = new Observing {}

  test("hasListeners") {
    val es = new EventSource[Nothing] {}
    es.hasListeners should equal (false)
    es.foreach{ _ => }
    es.hasListeners should equal (true)
  }

  test("flatMap (w/o initial)") {
    val parentES = new EventSource[Int] {}
    val childESs = (0 to 1) map { _ => new EventSource[Int] {} }
    val f = childESs.apply _
    val flatMapped = parentES flatMap f

    collecting(flatMapped)(childESs(0).fire(-1)) should equal (Nil)

    collecting(flatMapped)(parentES.fire(0)) should equal (Nil)
    collecting(flatMapped) {
      childESs(0).fire(1)
      childESs(0).fire(2)
    } should equal (List(1, 2))

    collecting(flatMapped)(parentES.fire(1)) should equal (Nil)
    collecting(flatMapped){
      childESs(1).fire(3)
      childESs(1).fire(4)
    } should equal (List(3, 4))
  }

  test("map") {
    val es = new EventSource[Int] {}
    val x = math.random * 100
    val f = (y: Int)  => (y * x).toInt
    val mapped = es map f

    collecting(mapped)(es fire 2) should equal (List(2) map f)
    collecting(mapped){
      es fire 3
      es fire 4
      es fire 5
    } should equal (List(3, 4, 5) map f)
  }

  test("filter") {
    val es = new EventSource[Int] {}
    val f = (_: Int) % 2 == 0
    val filtered = es filter f
    collecting(filtered)(es fire 2) should equal (List(2) filter f)
    collecting(filtered){
      es fire 3
      es fire 4
      es fire 5
    } should equal (List(3, 4, 5) filter f)
  }

  test("collect") {
    val es = new EventSource[Int] {}
    val pf: PartialFunction[Int, Int] = { case n if n > 10 => -n }
    val collected = es collect pf

    val values = List(5, 10, 11, 298)

    collecting(collected) {
      values foreach es.fire
    } should equal (values collect pf)
  }

  test("takeWhile") {
    val es = new EventSource[Int] {}
    val f = (_: Int) < 3
    val takenWhile = es takeWhile f
    collecting(takenWhile){
      es fire 2
      es fire 1
      es fire 4
      es fire 2
    } should equal (List(2, 1))
  }

  test("foldLeft") {
    val es = new EventSource[Int] {}
    val foldedLeft = es.foldLeft(20)(_ + _)
    collecting(foldedLeft)(es fire 1) should equal (List(21))
    collecting(foldedLeft)(es fire 2) should equal (List(23))
    collecting(foldedLeft)(es fire 13) should equal (List(36))
  }

  test("| (union)") {
    val es1, es2 = new EventSource[Int] {}
    val union = es1 | es2
    collecting(union){
      es1 fire 1
      es1 fire 2
      es2 fire 3
      es1 fire 4
    } should equal (List(1, 2, 3, 4))
  }

  test("hold") {
    val es = new EventSource[Int] {}
    val held = es.hold(72)
    held.now should equal (72)
    es fire 35
    held.now should equal (35)
    es fire 23
    held.now should equal (23)

  }

  test("garbage collection (foreach)") {
    def innerScope(observing1: Observing) = {
      val observing2 = new Observing {}
      val ret = new EventSource[Nothing] {}
      val f1, f2 = { _: Any => }
      val weakref1 = new scala.ref.WeakReference(f1)
      val weakref2 = new scala.ref.WeakReference(f2)
      ret.foreach(f1)(observing1)
      ret.foreach(f2)(observing2)
      (weakref1, weakref2, ret)
    }
    val observing1 = new Observing {}
    val (weakref1, weakref2, _) = innerScope(observing1)
    System.gc()
    if (weakref1.get.isEmpty) info("Warning - listener was gc'ed")
    if (weakref2.get.isDefined) info("Warning - listener was not gc'ed")
  }

  test("garbage collection (takeWhile)") {
    val es = new EventSource[Int]
    def makeTakenWhile = {
      val f = { (_: Int) < 3 }
      val takenWhile = es takeWhile f
      val weakref = new scala.ref.WeakReference(f)
      es fire 2
      System.gc()
      if (weakref.get.isEmpty) info("Warning - takeWhile EventSource was gc'ed")
      weakref
    }
    val weakref = makeTakenWhile
    es fire 10
    System.gc()
    if (weakref.get.isDefined) info("Warning - takeWhile EventSource was not gc'ed")
  }

  test("zipWithStaleness+nonblocking") {
    val es = new EventSource[Int]
    object last {
      var value = 0
    }
    es.zipWithStaleness.nonblocking.foreach {
      case (n, isStale) =>
        for (_ <- 1 to 10 if !isStale()) {
          last.synchronized {
            (n != last.value) should equal (isStale())
          }
          Thread.sleep(250)
        }
    }
    last.synchronized {
      last.value = 2
      es fire 2
    }
    Thread.sleep(250)
    last.synchronized {
      last.value = 3
      es fire 3
    }
    Thread.sleep(2250)
  }

  test("distinct") {
    val es = new EventSource[Int]
    collecting(es.distinct){
      es fire 0
      es fire 1
      es fire 1
      es fire 2
      es fire 3
      es fire 3
      es fire 2
    } should equal (List(0, 1, 2, 3, 2))
  }

  test("throttle") {
    Iterator
      .continually {
        val es = new EventSource[Int]
        val t = new es.Throttled(100)
        val start = System.currentTimeMillis
        es foreach { e =>
          info("Fired " + e + " at " + (System.currentTimeMillis - start))
        }
        def fireAndPause(e: Int, idealTime: Long, p: Long => Boolean): Boolean = {
          val a = System.currentTimeMillis
          es fire e
          while(System.currentTimeMillis - a < idealTime)
            Thread sleep 10
          p(System.currentTimeMillis - a)
        }
        val (collected, done) = collectingReturning(t) {
          fireAndPause(7, 50, _ < 100) &
            fireAndPause(4,  50,  _ < 100) &
            fireAndPause(9,  50,  _ < 100) &
            fireAndPause(13, 110, _ > 100) &
            fireAndPause(6,  110, _ > 100)
        }
        info("Finished collecting at " + (System.currentTimeMillis - start))
        if (done)
          collected should equal (List(13, 6))
        else
          info("Missed the boat, retrying")
        done
      }
      .dropWhile(_ == false)
      .next
  }
}

class SuppressableTests extends FunSuite with Matchers with CollectEvents {
  test("supressing") {
    val es = new Suppressable[Int] {}

    collecting(es)(es fire 1) should equal (List(1))
    collecting(es){
      es.suppressing {
        es fire 2
      }
    } should equal (Nil)

    collecting(es)(es fire 3) should equal (List(3))
  }
}
class BatchableTests extends FunSuite with Matchers with CollectEvents {
  test("batching") {
    val es = new Batchable[Int, Int] {}

    collecting(es) {
      es fire Include(0, 0)
      es.batching {
        es fire Include(0, 1)
      }
      es fire Include(0, 2)
      es.batching {
        es fire Include(0, 3)
        es fire Include(0, 4)
      }
      es fire Include(0, 5)
    } should equal (List(
      Include(0, 0),
      Include(0, 1),
      Include(0, 2),
      Batch(Include(0, 3), Include(0, 4)),
      Include(0, 5)
    ))
  }
}
