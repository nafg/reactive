package reactive
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.util.Random

import CollectEvents._


class ObservableBufferTests extends FunSuite with ShouldMatchers with Observing {
  test("consistency between mutations and applyDeltas") {
    val ob1 = new ObservableBuffer[Int]
    val ob2 = new ObservableBuffer[Int]
    ob1.messages foreach {delta =>
//      println("ob1: " + ob1)
//      println("ob2: " + ob2)
//      println("applying: " + delta)
      ob2.applyDelta(delta)
    }
    for(_ <- 1 to 100) {
      Random.nextInt(6) match {
        case 0 => ob1 += Random.nextInt
        case 1 => Random.nextInt +=: ob1
        case 2 => if(ob1.nonEmpty) ob1.insertAll(Random.nextInt(ob1.length), (0 to Random.nextInt(100)) map (_ => Random.nextInt))
        case 3 => if(ob1.nonEmpty) ob1.update(Random.nextInt(ob1.length), Random.nextInt)
        case 4 => if(ob1.nonEmpty) ob1.remove(Random.nextInt(ob1.length))
        case 5 => if(Random.nextBoolean) ob1.clear
      }
      ob1 should equal (ob2)
    }    
  }
}

class SeqSignalTests extends FunSuite with ShouldMatchers with Observing {
  test("map(TransformedSeq=>TransformedSeq)") {
    val ss = BufferSignal(1,2,3)
    val mapped = ss.map{ts =>
      ts.map(_ * 10)
    }
    collecting(mapped.deltas){
      ss.now += 4
    } should equal (List(
      Include(3, 40)
    ))
    mapped.now should equal (List(10,20,30,40))
  }
}

class BufferSignalTests extends FunSuite with ShouldMatchers with Observing {
  test("consistency") {
    val signal = BufferSignal(1,2,3)
    val ob = new ObservableBuffer[Int]
    ob += (1,2,3)
    var changes = 0
    var deltas = 0
//    signal.deltas foreach {d => println("delta: " + d)}
    signal.deltas foreach {d =>
      ob.applyDelta(d)
      signal.now should equal (ob)
      deltas += 1
    }
//    signal.change foreach {c => println("change: " + c)}
//    signal.change foreach {_ should equal (ob)}
    signal.change foreach {_ => changes += 1}
    
    signal.now += 4
    signal.now -= 3
    signal ()= Seq(10,9,8,7,6,5,4,3)
    
    signal.now should equal (ob)
    changes should equal (3)
    deltas should equal (3)
  }
}

