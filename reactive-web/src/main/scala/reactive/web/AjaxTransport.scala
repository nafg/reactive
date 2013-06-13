package reactive
package web

import net.liftweb.json.{ render => jrender, _ }
import net.liftweb.http.S

object AjaxTransport {
  def apply(page: Page): AjaxTransport = new AjaxTransport(page)
}
class AjaxTransport(page: Page) {
  class AjaxTask(key: Int, events: List[JValue]) {
    @volatile var done = false
    val scope = new LocalScope
    def run = page.inScope(scope){
      try {
        for (event <- events)
          event match {
            case JObject(JField(jsEventStreamId, eventJson) :: Nil) =>
              try {
                page.ajaxEvents.fire((jsEventStreamId, eventJson))
              } catch {
                case e: Exception => e.printStackTrace
              }
            case _ =>
              sys.error("Invalid reactive event: " + compact(jrender(event)))
          }
      } finally {
        done = true
        completed ::= (System.currentTimeMillis(), key)
      }
    }

    def js = scope.js.foldLeft(net.liftweb.http.js.JsCmds.Noop)(_ & _)
  }
  private val inProgress = new java.util.concurrent.ConcurrentHashMap[Int, AjaxTask]

  @volatile
  private var completed = List.empty[(Long, Int)]

  private val handler = S.SFuncHolder { s =>
    val (oldCompleted, recentCompleted) = completed partition (System.currentTimeMillis - _._1 > 60000)
    completed = recentCompleted
    oldCompleted foreach (inProgress remove _._2)
    parse(s) match {
      case JObject(List(JField("unique", JInt(unique)), JField("events", JArray(events)))) =>
        val key = unique.toInt
        inProgress.get(key) match {
          case null =>
            val task = new AjaxTask(key, events)
            inProgress.put(key, task)
            task.run
            task.js
          case task =>
            while (!task.done) Thread.sleep(30)
            task.js
        }

      case _ =>
        sys.error("Invalid reactive event json: " + s)
    }
  }

  val installJs =
    if (S.inStatefulScope_?)
      S.fmapFunc(S.contextFuncBuilder(handler)){ funcId =>
        "reactive.funcId='" + funcId + "'"
      }
    else
      "reactive.funcId='noStatefulScope'"
}
