package reactive
package web
package lift

import java.io.{ OutputStream, OutputStreamWriter, Writer }
import scala.annotation.tailrec
import net.liftweb.http.{ GetRequest, LiftRules, OutputStreamResponse, Req, S }
import net.liftweb.common.Full

/**
 * Contains the [[SseTransportType.init]] method which must
 * be invoked in `boot` for [[SseTransportType]] to work.
 */
object SseTransportType extends PagesCache {
  /**
   * Installs the SSE handler into Lift
   */
  def init(): Unit = {
    LiftRules.dispatch append {
      case req @ Req("__reactive-web-sse" :: PageById(page) :: Nil, "", GetRequest) =>
        S.respondAsync {
          page.transportTypes.collectFirst { case spc: SseTransportType => spc } map { spc =>
            OutputStreamResponse(
              (os: OutputStream) => {
                val osw = new OutputStreamWriter(os)
                spc.sseTransport.write(osw, req.header("Last-Event-ID") map (_.toLong) openOr 0)
                osw.close()
              },
              List(
                "Content-Type" -> "text/event-stream",
                "Cache-Control" -> "no-cache",
                "Connection" -> "keep-alive",
                "X-Accel-Buffering" -> "no"
              )
            )
          }
        }
    }
  }
}

/**
 * A [[Page]] that can push events to the browser via SSE (Server Side Events),
 * also known as HTML5 EventSource.
 */
class SseTransportType(page: Page) extends TransportType {
  /**
   * The longest duration of a connection
   */
  def maxTime = 600000

  object sseTransport extends Transport {
    case class Message(num: Long, data: String)
    class MRef[A](var value: Option[A]) {
      override def toString = s"MRef@$hashCode($value)"
    }
    class LinkedList[A](val elem: A, val next: MRef[LinkedList[A]] = new MRef[LinkedList[A]](None)) {
      override def toString = s"LinkedList($elem, $next)"
      @tailrec final def last: LinkedList[A] = next.value match {
        case None     => this
        case Some(ll) => ll.last
      }
    }

    private val allMessages = new MRef[LinkedList[Message]](None)

    private val counter = new AtomicRef(1)

    @tailrec
    def dropMessages(lastReceived: Long): Unit = allMessages.value match {
      case Some(ms) if ms.elem.num <= lastReceived =>
        allMessages.value = ms.next.value
        dropMessages(lastReceived)
      case _ =>
    }

    private[web] def write(writer: Writer, lastReceived: Long = 0): Unit = {
      val startTime = System.currentTimeMillis
      def remainingTime = maxTime - (System.currentTimeMillis - startTime)

      @tailrec
      def loop(next: MRef[LinkedList[Message]], last: Long): Unit = if(remainingTime > 0) {
        next.value match {
          case Some(ms) =>
            val msg = ms.elem
            writer.write(s"id: ${msg.num}\ndata: ${msg.data}\n\n")
            writer.flush()
            loop(ms.next, msg.num)
          case None =>
            synchronized {
              wait(1000)
            }
            loop(next, last)
        }
      }

      synchronized {
        dropMessages(lastReceived)
      }

      loop(allMessages, lastReceived)
    }

    def currentPriority = 10

    def queue[A](renderable: A)(implicit render: CanRender[A]): Unit = {
      val data = render(renderable)
      val num = counter.run(a => (a + 1, a))
      synchronized {
        val m = Some(new LinkedList(Message(num, data)))
        allMessages.value match {
          case None =>
            allMessages.value = m
          case Some(ms) =>
            ms.last.next.value = m
        }
        notify()
      }
    }
  }

  override def render = super.render ++ <script type="text/javascript">
    reactive.sse = new EventSource('/__reactive-web-sse/{ page.id }');
    reactive.sse.addEventListener('message', function(e) {{
      eval(e.data);
    }}, false)
  </script>

  linkTransport(sseTransport)

  SseTransportType.addPage(page)
}
