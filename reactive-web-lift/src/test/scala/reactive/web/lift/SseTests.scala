package reactive
package web
package lift

import org.scalatest.concurrent.Eventually
import org.scalatest.time.SpanSugar
import scala.concurrent._
import ExecutionContext.Implicits._
import java.io._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SseTests extends AnyFunSuite with Matchers with Eventually with SpanSugar {
  test("SseTransportType") {
    object page extends Page {
      val spc = new SseTransportType(this) {
        override def maxTime = 2000
      }
      val transportTypes = spc :: Nil
    }
    val sw = new StringWriter
    val fut = Future {
      page.spc.sseTransport.write(sw)
    }
    Thread.sleep(200)
    page.spc.sseTransport.queued fire StringRenderable("hello\nworld")
    Await.ready(fut, duration.Duration.Inf)

    eventually(timeout(3000.millis)) {
      sw.toString should equal ("id: 1\ndata: hello\ndata: world\n\n")
    }
  }
}
