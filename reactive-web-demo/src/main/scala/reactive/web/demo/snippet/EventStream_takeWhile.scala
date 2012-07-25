package reactive
package web
package demo
package snippet

class EventStream_takeWhile extends EventStreamDemo {
  
  override lazy val eventStream = {
    var stop = false
    eventSource.takeWhile{
      case "stop" => false   // Stop immediately, functional
      case "next" if !stop =>  // Not encouraged, stop via side effect
        stop = true
        true
      case _ => !stop
    }
  }
}
