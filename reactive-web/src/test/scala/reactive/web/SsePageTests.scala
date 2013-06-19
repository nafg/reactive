package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.concurrent.Eventually
import org.scalatest.time.SpanSugar

import scala.concurrent._
import ExecutionContext.Implicits._

import java.io._

class SsePageTests extends FunSuite with ShouldMatchers with Eventually with SpanSugar {
  test("SsePage") {
    val page = new SsePage {
      override def maxTime = 2000
    }
    val sw = new StringWriter
    val fut = future {
      page.sseTransport.write(sw)
    }
    Thread.sleep(200)
    page.sseTransport.queue("hello")
    Await.ready(fut, duration.Duration.Inf)
    
    eventually(timeout(3000.millis)) {
      sw.toString should equal ("id: 1\ndata: hello\n\n")
    }
  }
}
