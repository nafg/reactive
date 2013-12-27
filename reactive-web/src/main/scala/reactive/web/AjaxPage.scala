package reactive
package web

import net.liftweb.json._

/**
 * `AjaxPage` extends [[Page]] with the ability
 * to send javascript to the browser as a response
 * to ajax requests.
 * The actual installation and linking of ajax handlers
 * is left to implementors.
 */
trait AjaxPageComponent extends PageComponent {
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
          try events foreach {
            case JObject(JField(jsEventStreamId, eventJson) :: Nil) =>
              try ajaxEvents.fire((jsEventStreamId, eventJson))
              catch {
                case e: Exception => e.printStackTrace
              }
            case event =>
              sys.error("Invalid reactive event format: " + compact(JsonAST.render(event)))
          } finally {
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
      accum.data
    }
  }

  protected val tasks = new AtomicRef(List.empty[AjaxTask])

  /**
   * Process an incoming ajax request.
   * The data will be fired from `ajaxEvents`, but
   * that is delegated to an [[AjaxTask]]. If the same
   * ajax call is retried we will reuse the same [[AjaxTask]]
   * so work is not done twice.
   */
  protected def handleAjax(json: =>JValue): Seq[String] = try {
    tasks.transform(_ filter (_.completionTime map (System.currentTimeMillis - _ > 60000) getOrElse true))
    json match {
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
      case other =>
        sys.error("Invalid reactive json format: " + compact(JsonAST.render(other)))
    }
  } catch {
    case e: Exception =>
      e.printStackTrace
      Nil
  }
}
