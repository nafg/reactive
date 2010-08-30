package reactive

import org.scalatest.{FunSuite, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers


object CollectEvents extends CollectEvents
//TODO fold trait into singleton, and change inheritance to imports
trait CollectEvents {
  def collecting[T](es: EventStream[T])(f: =>Unit): List[T] = {
    var log = List[T]()
    var executing = true
    implicit val observing = new Observing {}
    es.takeWhile(_ => executing).foreach{e=>log:+=e}(observing)
    f
    executing = false
    log
  }  
}

class EventStreamTests extends FunSuite with ShouldMatchers with CollectEvents {
  implicit val observing = new Observing {}
  
  test("hasListeners") {
    val es = new EventStream[Nothing] {}
    es.hasListeners should equal (false)
    es.foreach{_ => }
    es.hasListeners should equal (true)
  }
 
  test("flatMap (w/o initial)") {
    val parentES = new EventStream[Int] {}
    val childESs = (0 to 1) map {_ => new EventStream[Int] {} }
    val f = childESs.apply _
    val flatMapped = parentES flatMap f
    
    collecting(flatMapped)(childESs(0).fire(-1)) should equal (Nil)
    
    collecting(flatMapped)(parentES.fire(0)) should equal (Nil)
    collecting(flatMapped) {
      childESs(0).fire(1)
      childESs(0).fire(2)
    } should equal (List(1,2))
    
    collecting(flatMapped)(parentES.fire(1)) should equal (Nil)
    collecting(flatMapped){
      childESs(1).fire(3)
      childESs(1).fire(4)
    } should equal (List(3,4)) 

  }
  
  test("map") {
    val es = new EventStream[Int] {}
    val x = math.random*100
    val f = (_:Int)*x toInt
    val mapped = es map f
    
    collecting(mapped)(es fire 2) should equal (List(2) map f)
    collecting(mapped){
      es fire 3
      es fire 4
      es fire 5
    } should equal (List(3,4,5) map f)
  }
  
  test("filter") {
    val es = new EventStream[Int] {}
    val f = (_:Int) % 2 == 0 
    val filtered = es filter f
    collecting(filtered)(es fire 2) should equal (List(2) filter f)
    collecting(filtered){
      es fire 3
      es fire 4
      es fire 5
    } should equal (List(3,4,5) filter f)
  }
  
  test("takeWhile") {
    val es = new EventStream[Int] {}
    val f = (_:Int) < 3
    val takenWhile = es takeWhile f
    collecting(takenWhile){
      es fire 2
      es fire 1
      es fire 4
      es fire 2
    } should equal (List(2,1))
  }
  
  test("foldLeft") {
    val es = new EventStream[Int] {}
    val foldedLeft = es.foldLeft(20)(_ + _)
    collecting(foldedLeft)(es fire 1) should equal (List(21))
    collecting(foldedLeft)(es fire 2) should equal (List(23))
    collecting(foldedLeft)(es fire 13) should equal (List(36))
  }
  
  test("| (union)") {
    val es1, es2 = new EventStream[Int] {}
    val union = es1 | es2
    collecting(union){
      es1 fire 1
      es1 fire 2
      es2 fire 3
      es1 fire 4
    } should equal (List(1,2,3,4))
  }
  
  test("hold") {
    val es = new EventStream[Int] {}
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
      val ret = new EventStream[Nothing] {}
      val f1,f2 = {_:Any => }
      val weakref1 = new scala.ref.WeakReference(f1)
      val weakref2 = new scala.ref.WeakReference(f2)
      ret.foreach(f1)(observing1)
      ret.foreach(f2)(observing2)
      (weakref1, weakref2, ret)
    }
    val observing1 = new Observing {}
    var (weakref1, weakref2, es) = innerScope(observing1)
    System.gc
    if(weakref1.get.isEmpty) info("Warning - listener was gc'ed")
    if(weakref2.get.isDefined) info("Warning - listener was not gc'ed")
  }
  
  test("garbage collection (takeWhile)") {
    val es = new EventStream[Int] {}
    def makeTakenWhile = {
      val takenWhile = es takeWhile (_ < 3)
      new scala.ref.WeakReference(takenWhile)
    }
    val weakref = makeTakenWhile
    es fire 2
    System.gc
    if(weakref.get.isEmpty) info("Warning - takeWhile EventStream was gc'ed")
    es fire 10
    System.gc
    if(weakref.get.isDefined) info("Warning - takeWhile EventStream was not gc'ed")
  }
}


class SuppressableTests extends FunSuite with ShouldMatchers with CollectEvents {
  test("supressing") {
    implicit val observing = new Observing {}
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
class BatchableTests extends FunSuite with ShouldMatchers with CollectEvents {
  test("batching") {
    implicit val observing = new Observing {}
    val es = new Batchable[Int,Int] {}
    
    collecting(es) {
      es fire Include(0,0)
      es.batching {
        es fire Include(0,1)
      }
      es fire Include(0,2)
      es.batching {
        es fire Include(0,3)
        es fire Include(0,4)
      }
      es fire Include(0,5)
    } should equal (List(
      Include(0,0),
      Include(0,1),
      Include(0,2),
      Batch(Include(0,3), Include(0,4)),
      Include(0,5)
    ))
  }
}
