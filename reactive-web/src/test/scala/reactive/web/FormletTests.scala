package reactive
package web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks

import org.scalacheck.{ Arbitrary, Gen }

import scala.xml._

class FormletTests extends FunSuite with ShouldMatchers with PropertyChecks with Observing {
  implicit def genElem: Gen[Elem] = Gen.oneOf(<div/>, <span/>, <form/>, <fieldset/>) map { e =>
    e.copy(child = Gen.listOf(genXml)(Gen.Params(7)) map (_.flatten) getOrElse Nil)
  }
  implicit def genXml: Gen[NodeSeq] = Gen.frequency(
    (1, genElem),
    (4, Gen.alphaStr map (Text(_)))
  )
  implicit val arb = Arbitrary(genXml)

  test("Formlet.pure") {
    forAll{ (a: String, ns: NodeSeq) =>
      val f = Formlet.pure(a)
      val r = f.rendering
      implicitly[r.type <:< List[Nothing]]
      f.signal.now should equal (a)
    }
  }

  test("Formlet#<*>") {
    val f = (
      Formlet.pure{ x: Int => y: String => y * x }
      <*> Formlet.pure(5)
      <*> Formlet.pure("x")
    )
    f.signal.now should equal ("x" * 5)
  }

  test("Formlet#flatMap") {
    val v = Var(10)
    implicit val page = new Page
    Page.withPage(page) {
      val f = for {
        (n: Int) <- Formlet(v, Nil)
        x <- Formlet(Val("x" * n), List((_: NodeSeq) => Text("x" * n)))
      } yield x
      val ts = new TestScope(f.render apply <div/>)
      import ts._
      Reactions.inScope(ts) {
        ts.xml should equal (<div id="reactiveWebId_000000">{ "x" * v.now }</div>)
        for (_ <- 0 to 100) {
          val i = math.random * 100 toInt;
          v () = i
          ts.xml should equal (<div id="reactiveWebId_000000">{ "x" * v.now }</div>)
        }
      }
    }
  }

  test("Formlets.value") {
    implicit val observing = new Observing {}
    var v = "abc"
    val page = new Page
    Page.withPage(page) {
      val f = Formlets.value(v) map { x =>
        v = x
      }
      val ts = new TestScope(f.rendering(0)(<input id="myid"/>))
      import ts._
      ts("myid").value should equal (v)
      forAll(){ (s: String) =>
        Page.withPage(page) {
          Reactions.inScope(ts) {
            (ts("myid").value = s) fire Change()
            v should equal (s)
          }
        }
      }
    }
  }

  test("Formlets.select") {
    implicit val observing = new Observing {}
    val choices = List('option1, 'option2, 'option3)
    var v: Symbol = choices.head

    val page = new Page
    Page.withPage(page) {
      val f = Formlets.select(choices)(v) map (v = _)
      val ts = new TestScope(f.rendering(0)(<select id="myid"/>))
      import ts._
      forAll(Gen.oneOf(choices.zipWithIndex.map(_._2))) { i =>
        Page.withPage(page) {
          Reactions.inScope(ts) {
            (ts("myid")("selectedIndex") = i) fire Change()
            v should equal (choices(i))
          }
        }
      }
    }
  }

  test("Formlets.seq") {
    Page.withPage(new Page) {
      val formlet = ".xs" #: Formlets.seq {
        List(true, false, false, true).map { x =>
          ".checkbox" #: Formlets.checkbox(x) <*
            ".label *" #: Formlets.text(x.toString)
        }: _*
      }
      println(formlet.render apply <label class="xs">
                                     <input type="checkbox" class="checkbox"/>
                                     <span class="label"></span>
                                   </label>)
    }
  }
}
