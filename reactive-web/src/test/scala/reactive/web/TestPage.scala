package reactive
package web

import scala.xml._

class TestPage(xmlFunc: Page => Node = _ => Group(Nil)) extends Page {
  val testTransport = {
    val nullTransport = new AccumulatingTransport { def currentPriority = 0 }
    val xml = withTransport(nullTransport) { xmlFunc(this) }
    new TestTransport(xml)(this)
  }
  linkTransport(testTransport)

  def collectQueued(f: =>Unit): Seq[String] = testTransport.collectQueued(f)
}
