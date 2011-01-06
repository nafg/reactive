package reactive

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class SignalTests extends FunSuite with ShouldMatchers with CollectEvents {
  def dumpListeners(es: EventStream[_]) = es match {case es: EventSource[_] => es.dumpListeners}
  
  implicit val observing = new Observing {}
  test("map") {
    val s = Var(10)
    val mapped = s.map[Int, Signal[Int]](_ * 3)
    
    mapped.now should equal (30)
    
    collecting(mapped.change) {
      s ()= 60
      mapped.now should equal (180)
    } should equal (List(180))
  }
  
  test("flatMap (regular T=>Signal[U])") {
    val aVar = Var(3)
    val vals = List(Val(1), Val(2), aVar)
    val parent = Var(0)
    val flatMapped = parent.flatMap[Int, Signal](vals)

    collecting(flatMapped.change) {
      flatMapped.now should equal (1)
      
      parent ()= 1
      flatMapped.now should equal (2)
      
      parent ()= 2
      flatMapped.now should equal (3)
      aVar () = 4
      flatMapped.now should equal (4)
      
      parent ()= 0
      flatMapped.now should equal (1)
    
    } should equal (List(2,3,4,1))
  }
  
  test("flatMap (T=>SeqSignal[U])") {
//    EventSource.debug = true
    val bufSig1 = BufferSignal(1,2,3)
    val bufSig2 = BufferSignal(2,3,4)
    val parent = Var(false)
    val flatMapped: SeqSignal[Int] = parent.flatMap {b: Boolean =>
      if(!b) bufSig1 else bufSig2
    }
    
    flatMapped.now should equal (Seq(1,2,3))
    
    collecting(flatMapped.deltas){
      collecting(flatMapped.change){
        parent.change.asInstanceOf[EventSource[Boolean]].dumpListeners
        System.gc()
        parent.change.asInstanceOf[EventSource[Boolean]].dumpListeners
//        println("Setting to true...")
        parent ()= true
//        println("done")
//        System.gc()
        flatMapped.now should equal (Seq(2,3,4))
      } should equal (List(List(2,3,4)))
    }.toBatch.applyToSeq(List(1,2,3)) should equal (List(2,3,4))
    
    collecting(flatMapped.deltas){
      collecting(flatMapped.change){
        bufSig2.deltas.asInstanceOf[EventSource[_]].debug=true
        bufSig2.change.asInstanceOf[EventSource[_]].debug=true
//        dumpListeners(bufSig2.deltas)
        System.gc
//        dumpListeners(bufSig2.deltas)
        bufSig2 ()= Seq(2,3,4,5)
        flatMapped.now should equal (Seq(2,3,4,5))
//        dumpListeners(bufSig2.deltas)
      } should equal (List(List(2,3,4,5)))
    } should equal (List(
      Batch(Include(3,5))
    ))
    
    
    collecting(flatMapped.deltas){
      collecting(flatMapped.change){
        parent ()= false
        flatMapped.now should equal (Seq(1,2,3))    
      } should equal (List(List(1,2,3)))
    
    }.toBatch.applyToSeq(List(2,3,4,5)) should equal (List(1,2,3))
    
  }

  test("flatMap(value => seqSignal.map(_ filter pred))") {
    val switch = Var(false)
    val numbers = BufferSignal(1,2,3,4,5)
    val filteredNumbers = numbers.map(_ filter (_ < 3))
    val flatMapped = switch.flatMap {
      case false => numbers
      case true => filteredNumbers
    }
    
    numbers.now.baseDeltas should equal (Seq(
      Include(0,1), Include(1,2), Include(2,3), Include(3,4), Include(4,5)
    ))
    filteredNumbers.now.baseDeltas should equal (Seq(
      Remove(2,3), Remove(2,4), Remove(2,5)
    ))
    
    collecting(flatMapped.deltas) {
      switch ()= true
    } should equal (List(
      Batch(
        Remove(2,3), Remove(2,4), Remove(2,5)
      )
    ))
  }
}

object Run {
  def main(args: Array[String]) = new SignalTests().execute()
}