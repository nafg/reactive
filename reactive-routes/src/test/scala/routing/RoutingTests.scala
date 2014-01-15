package reactive
package routing

import org.scalatest.{ FunSuite, Inside }
import org.scalatest.Matchers

class RoutingTests extends FunSuite with Matchers with Inside {
  def printType[A : Manifest](a: A) = println(s"$a : ${manifest[A]}")

  implicit object intStringable extends Stringable[Int] {
    def format = _.toString
    def parse = s => try Some(s.toInt) catch { case _: Exception => None }
  }

  val p = "add" :/: arg[Int] :/: "plus" :/: arg[Int] :/: "please"
  val p2 = "addall" :/: **
  val p3 = "test" :/: arg[Int] :&: param[Int]("test1") :&: param[Int]("test2") :&: params[Int]("test3")

  val p4 = "test" :/: arg[Int]
  val p5 = "test" :&: param[Int]("test1")

  val r = p >> { a => b => a + b }
  val r2 = p2 >> { xs => xs.map(intStringable.parse).flatten.sum }
  val r3 = p3 >> { x => test1 => test2 => test3 => s"test/$x?test1=$test1&test2=$test2" }

  test("Constructing links") {
    val c = p.construct(10)(30)
    c.path should equal ("add" :: "10" :: "plus" :: "30" :: "please" :: Nil)
    c.query should equal (Nil)

    val c2 = p2.construct("1" :: "2" :: Nil)
    c2.path should equal ("addall" :: "1" :: "2" :: Nil)
    c2.query should equal (Nil)

    val c3 = p3.construct(10)(20)(30)(List(1,2,3,4))
    c3.path should equal ("test" :: "10" :: Nil)
    c3.query should equal (("test3", "1") :: ("test3", "2") :: ("test3", "3") :: ("test3", "4") :: ("test2", "30") :: ("test1", "20") :: Nil)
  }

  test("Running urls") {
    r.run(Location("add" :: "10" :: "plus" :: "20" :: "please" :: Nil)) should equal (30)
    r.run.isDefinedAt(Location("add" :: "10" :: "and" :: "20" :: "please" :: Nil)) should equal (false)
    r.run.isDefinedAt(Location("add" :: "10" :: "plus" :: "20" :: Nil)) should equal (false)

    List(
      Location(Nil),
      Location("addall" :: Nil),
      Location("addall" :: "1" :: Nil),
      Location("addall" :: "1" :: "2" :: Nil),
      Location("addall" :: "1" :: "2" :: "3" :: Nil)
    ).map(r2.run.lift) should equal (List(
      None,
      Some(0),
      Some(1),
      Some(3),
      Some(6)
    ))

    r3.run(
      Location("test" :: "10" :: Nil, List("test1" -> "20", "test2" -> "30", "test2" -> "40", "test3" -> "1", "test3" -> "2", "test3" -> "a"))
    ) should equal ("test/10?test1=20&test2=30")
    r3.run.isDefinedAt(Location("test" :: "10" :: Nil, List("test1" -> "20", "test2" -> "X30", "test2" -> "40", "test3" -> "1", "test3" -> "2"))) should equal (false)
  }

  test("Sites") {
    val s = r & r2 & r3

    List(
      Location("add" :: "10" :: "plus" :: "20" :: "please" :: Nil),
      Location("add" :: "10" :: "and" :: "20" :: "please" :: Nil),
      Location("add" :: "10" :: "plus" :: "20" :: Nil),
      Location(Nil),
      Location("addall" :: Nil),
      Location("addall" :: "1" :: Nil),
      Location("addall" :: "1" :: "2" :: Nil),
      Location("addall" :: "1" :: "2" :: "3" :: Nil),
      Location("test" :: "10" :: Nil, List("test1" -> "20", "test2" -> "30", "test2" -> "40", "test3" -> "1", "test3" -> "2", "test3" -> "a")),
      Location("test" :: "10" :: Nil, List("test1" -> "20", "test2" -> "X30", "test2" -> "40", "test3" -> "1", "test3" -> "2"))
    ).map(s.run.lift) should equal (List(
      Some(30),
      None,
      None,
      None,
      Some(0),
      Some(1),
      Some(3),
      Some(6),
      Some("test/10?test1=20&test2=30"),
      None
    ))
  }

  test("Constructing locations from sites") {
    val s = r & r
    val xs = s.construct map (_(10)(20))
    xs.head.path should equal ("add" :: "10" :: "plus" :: "20" :: "please" :: Nil)
  }

  test("mapPath") {
    val site = r & r

    val z = site mapPath ("test" :/: _) by { x => x}
    val l = z.construct.map(_(10)(20))
    l.head should equal (Location("test" :: "add" :: "10" :: "plus" :: "20" :: "please" :: Nil, Nil))

    val zz = site.mapPath(arg[Int] :/: _) by { f => a => b => c => f(a)(b) + c }
    val ll = zz.construct.map(_(10)(20)(30))
    ll.head should equal (Location("10" :: "add" :: "20" :: "plus" :: "30" :: "please" :: Nil, Nil))

    zz.run(Location("10" :: "add" :: "20" :: "plus" :: "30" :: "please" :: Nil, Nil)) should equal (60)
  }

  test("map") {
    val site = r & r
    val z =  site map ("000" + _)

    val l = z.construct.map(_(10)(20))
    l should equal (site.construct.map(_(10)(20)))

    val res = z.run(l.head)

    implicitly[res.type <:< String]

    res should equal ("00030")
  }
}
