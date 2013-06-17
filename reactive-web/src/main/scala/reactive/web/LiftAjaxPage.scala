package reactive
package web

import net.liftweb.json.{ render => jrender, _ }
import net.liftweb.http.S._

object LiftAjaxPage {
  def apply() = new AppendToRenderPage with LiftAjaxPage { }
}

trait LiftAjaxPage extends Page {
  class AjaxTask(val key: Long, events: List[JValue]) {
    @volatile private var started = false

    @volatile var completionTime: Option[Long] = None

    object accum extends AccumulatingTransport {
      def currentPriority = 50
    }

    /**
     * Runs this task if it hasn't been run yet.
     * If `runOnce` has already been invoked, does nothing.
     */
    def runOnce() = synchronized {
      if(!started) {
        started = true
        withTransport(accum) {
          try
            events foreach {
              case JObject(JField(jsEventStreamId, eventJson) :: Nil) =>
                try
                  ajaxEvents.fire((jsEventStreamId, eventJson))
                catch {
                  case e: Exception => e.printStackTrace
                }
              case event =>
                sys.error("Invalid reactive event format: " + compact(jrender(event)))
            }
          finally {
            completionTime = Some(System.currentTimeMillis)
            notifyAll()
          }
        }
      }
    }

    /**
     * Returns the javascript after `runOnce` has completed, blocking if necessary
     */
    def take = synchronized {
      while(completionTime.isEmpty)
        wait()
      net.liftweb.http.js.JsCmds.Run(accum.data.mkString(";\n"))
    }
  }

  private val tasks = new AtomicRef(List.empty[AjaxTask])

  private val handler = { urlParam: String =>
    tasks.transform(_ filter (_.completionTime map (System.currentTimeMillis - _ > 60000) getOrElse true))
    parse(urlParam) match {
      case JObject(List(JField("unique", JInt(key)), JField("events", JArray(events)))) =>
        lazy val newTask = new AjaxTask(key.toLong, events)
        val task = tasks.run { ts =>
          ts.find(_.key == key.toLong) match {
            case Some(oldTask) => (ts,            oldTask)
            case None          => (newTask :: ts, newTask)
          }
        }
        task.runOnce()
        task.take
      case _ =>
        sys.error("Could not parse reactive event json: " + urlParam)
    }
  }

  override def render = super.render ++
    <script type="text/javascript">{
      fmapFunc(contextFuncBuilder(SFuncHolder(handler))){ "reactive.funcId='" + _ + "'" }
    }</script>
}
