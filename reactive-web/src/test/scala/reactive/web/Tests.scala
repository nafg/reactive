package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.mockweb._
import scala.xml.{ Elem, NodeSeq, Text, UnprefixedAttribute, Null }
import org.scalatest.concurrent.Conductor
import java.lang.Thread
import scala.concurrent.SyncVar
import net.liftweb.util.Helpers._
import org.scalatest.prop.PropertyChecks

class RElemTests extends FunSuite with ShouldMatchers {
  test("Rendering an RElem to an Elem with an id should retain that id") {
    Page.withPage(new Page) {
      val elem = <anElem id="anId"/>
      val rElem = RElem(<span>A span</span>)
      rElem(elem).asInstanceOf[Elem].attribute("id").map(_.text) should equal(Some("anId"))
    }
  }
}
class RepeaterTests extends FunSuite with ShouldMatchers with PropertyChecks {
  test("Repeater should render its children") {
    Page.withPage(new Page) {
      val select = html.Select(Val(List(1, 2, 3)))(new Observing {}, Config.defaults)
      select(<select/>).asInstanceOf[Elem].child.length should equal (3)
    }
  }

  test("Repeater should send correct deltas") {
    MockWeb.testS("/") {
      val template = <div><span></span></div>
      implicit val page = Page.currentPage
      import org.scalacheck.Gen._
      forAll(listOf1(listOf(alphaUpperChar map (_.toString))), maxSize(10)){ xss: List[List[String]] =>
        whenever(xss.length >= 2) {
          val signal = BufferSignal(xss.head: _*)
          def snippet: NodeSeq => NodeSeq = "div" #> Repeater(signal.now.map(x => "span *" #> x).signal)
          val ts = new TestScope(snippet(template))
          import ts._
          Page.withPage(page) {
            Reactions.inScope(ts) {
              //              println(Console.RESET+"\n"+"=" * 25)
//              println(xss)
              for (xs <- xss.tail) {
                try {
                  signal () = xs
                  //                  println(ts.js)
                  //                  println(ts.xml)
                  (ts.xml >+ "span" length) should equal (signal.now.length)
                  (ts.xml >+ "span" map (_.text)) should equal (signal.now)
                } catch {
                  case e =>
                    println(Console.RED + e)
                    println("X" * 25 + Console.RESET)
                    throw e
                }
              }
              //              println(Console.GREEN+"=" * 10+" Ok "+"=" * 11 + Console.RESET)
            }
          }
        }
      }
    }
  }
}

class DomPropertyTests extends FunSuite with ShouldMatchers {
  test("DomProperty has one id per page") {
    Page.withPage(new Page) {
      val property = DomProperty("someName")
      val e1 = property.render apply <elem1/>
      val e2 = property.render apply <elem2/>
      e1.attributes("id") should equal(e2.attributes("id"))
    }
  }

  test("DomProperty updates go everywhere except same property on same page") {
    implicit val o = new Observing {}
    val prop1, prop2 = DomProperty("prop") withEvents DomEventSource.change
    prop1.values >> prop2
    //TODO testable comet should be in the library
    class TestPage(xml: => NodeSeq) extends Page {
      override lazy val comet = new ReactionsComet {
        override def queue[T](renderable: T)(implicit canRender: CanRender[T]) {
          ts.queue(renderable)
        }
      }
      val ts = new TestScope(Page.withPage(this)(xml))
    }
    val pageA, pageB = new TestPage((prop1.render apply <elem1/>) ++ (prop2.render apply <elem2/>))

    def thread(n: String)(f: => Unit) = new Thread(n) {
      val exc = new SyncVar[Option[Throwable]]
      start

      override def run = try {
        f
        println(this+" ok")
        exc.put(None)
      } catch {
        case e =>
          println(this+":")
          e.printStackTrace()
          exc.put(Some(e))
      } finally {
        println(this+" finally")
      }
    }
    val sync = new SyncVar[Unit]

    val ta = thread("pageA") {
      val ts = pageA.ts
      import ts._

      Page.withPage(pageA) {
        Reactions.inScope(ts) {
          println("pageA part one")
          val t = System.currentTimeMillis()
          ((ts / "elem1")("prop") = "value1") fire Change()
          println("Finished firing Change after "+(System.currentTimeMillis() - t))
          ts / "elem1" attr "prop" should equal ("value1")
          ts / "elem2" attr "prop" should equal ("value1")
          sync put ()
          while (sync.isSet) Thread.`yield`()
          sync get 2000 getOrElse error("timeout waiting for pageB")
          sync.unset

          println("pageA part two")
          ts / "elem1" attr "prop" should equal ("value2")
          ts / "elem2" attr "prop" should equal ("value2")
          println("pageA done")
        }
      }
      println("thread pageA done")
    }
    val tb = thread("pageB") {
      val ts = pageB.ts
      import ts._

      sync get 10000 getOrElse error("timeout waiting for pageA")
      sync.unset
      Page.withPage(pageB) {
        Reactions.inScope(ts) {
          println("pageB")
          ts / "elem2" attr "prop" should equal ("value1")
          ts / "elem1" attr "prop" should equal ("value1")

          ((ts / "elem1")("prop") = "value2") fire Change()
          ts / "elem1" attr "prop" should equal ("value2")
          ts / "elem2" attr "prop" should equal ("value2")
          println("pageB done")
        }
      }
      sync put ()
      println("thread pageB done")
    }
    ta.exc.get(10000) orElse tb.exc.get(10000) match {
      case Some(Some(e)) => throw e
      case None          => error("timeout")
      case Some(None)    =>
    }
  }
}

class DomEventSourceTests extends FunSuite with ShouldMatchers {
  test("DomEventSource only renders the current Page's propagation javascript") {
    MockWeb.testS("/") {
      val property = DomProperty("someName") withEvents DomEventSource.click
      val e1 = Page.withPage(new Page)(property.render apply <elem1/>)
      val e2 = Page.withPage(new Page)(property.render apply <elem1/>)
      ((e1 \ "@onclick" text) split ";" length) should equal (3)
      ((e2 \ "@onclick" text) split ";" length) should equal (3)
    }
  }
}

class TestScopeTests extends FunSuite with ShouldMatchers with Observing {
  test("TestScope") {
    MockWeb.testS("/") {
      val template = <span id="span">A</span>
      val signal = Var("A")
      def snippet: NodeSeq => NodeSeq =
        "span" #> Cell{ signal map { s => { ns: NodeSeq => Text(s) } } }
      val xml = Reactions.inScope(new TestScope(snippet apply template)) {
        signal () = "B"
      }.xml
      (xml \\ "span" text) should equal ("B")
      xml.toString should equal (<span id="span">B</span> toString)
    }
  }

  test("Emulate event") {
    Page.withPage(new Page) {
      var fired = false
      val event = DomEventSource.keyUp ->> { fired = true }
      val input = event.render apply <input/>
      val ts = new TestScope(input)
      import ts._
      input fire KeyUp(56)
      fired should equal (true)
    }
  }

  test("Emulate property change") {
    Page.withPage(new Page) {
      val value = PropertyVar("value")("initial") withEvents DomEventSource.change
      val input = value render <input id="id"/>
      val ts = new TestScope(input)
      import ts._
      (input("value") = "newValue") fire Change()
      value.now should equal ("newValue")
    }
  }

  test("Confirm") {
    val ts = new TestScope(<xml/>)
    import ts._
    implicit val page = new Page
    println(page.id)
    Reactions.logLevel = Logger.Levels.Trace
    Logger.traces ?>> {
      case Reactions.LogEvent(_, q @ Reactions.QueueingJS(p, _)) =>
        println(p + ": " + q.js)
    }
    Reactions.inScope(ts) {
      var result: Option[Boolean] = None
      Page.withPage(page) { //FIXME
        confirm("Are you sure?") { case b => result = Some(b) }
      }
      ts.takeConfirm match {
        case Some((msg, f)) =>
          msg should equal("Are you sure?")
          f(true)
          result should equal(Some(true))
          f(false)
          result should equal(Some(false))
      }
    }
  }
}

class DomMutationTests extends FunSuite with ShouldMatchers {
  import DomMutation._

  test("Apply to xml") {
    val template = <elem><parent id="parentId"><child1 id="child1"/><child2 id="child2"/></parent></elem>
    InsertChildBefore("parentId", <child3/>, "child2") apply template should equal (
      <elem><parent id="parentId"><child1 id="child1"/><child3/><child2 id="child2"/></parent></elem>
    )
    AppendChild("parentId", <child3/>) apply template should equal (
      <elem><parent id="parentId"><child1 id="child1"/><child2 id="child2"/><child3/></parent></elem>
    )
    RemoveChild("parentId", "child1") apply template should equal (
      <elem><parent id="parentId"><child2 id="child2"/></parent></elem>
    )
    ReplaceChild("parentId", <child3/>, "child1") apply template should equal (
      <elem><parent id="parentId"><child3/><child2 id="child2"/></parent></elem>
    )
    ReplaceAll("parentId", <child3/> ++ <child4/>) apply template should equal (
      <elem><parent id="parentId"><child3/><child4/></parent></elem>
    )
    val up = UpdateProperty("parentId", "value", "value", 30) apply template
    up.asInstanceOf[Elem].child(0).attributes.asAttrMap should equal (Map("id" -> "parentId", "value" -> "30"))
  }

  test("Rendering") {
    defaultDomMutationRenderer(InsertChildBefore("parentId", <elem/>, "beforeId")) should equal (
      """reactive.insertChild('parentId',reactive.createElem('elem',{},""),'beforeId')"""
    )
  }
}
