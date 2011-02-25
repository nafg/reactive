package reactive.web.snippet

import reactive._



class EventStream_foldLeft extends EventStreamDemo {
  case class AvgState(total: Double, count: Int)
  override lazy val eventStream =
    eventSource.foldLeft(AvgState(0,0)){
      case (AvgState(total, count), s) => AvgState(total+s.length,count+1)
    } map {
      case AvgState(total, count) =>
        "Average length so far: " + (total/count)
    }
}
