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
  implicit object strStringable extends Stringable[String] {
    def format = identity
    def parse = Some(_)
  }
  implicit object boolStringable extends Stringable[Boolean] {
    def format = _.toString
    def parse = {
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
    }
  }

  val p = "add" :/: arg[Int] :/: "plus" :/: arg[Int] :/: "please"
  val p2 = "addall" :/: **
  val p3 = "test" :/: arg[Int] :&: param[Int]("test1") :&: param[Int]("test2") :&: params[Int]("test3")

  val p4 = "test" :/: arg[Int]
  val p5 = "test" :&: param[Int]("test1")

  val r = p >> { a => b => a + b }
  val r2 = p2 >> { xs => xs.map(intStringable.parse).flatten.sum }
  val r3 = p3 >> { x => test1 => test2 => test3 => (x, test1, test2, test3) }

  test("Constructing links") {
    val c = p.construct(10)(30)
    c.path should equal ("add" :: "10" :: "plus" :: "30" :: "please" :: Nil)
    c.query should equal (Nil)

    val c2 = p2.construct("1" :: "2" :: Nil)
    c2.path should equal ("addall" :: "1" :: "2" :: Nil)
    c2.query should equal (Nil)

    val c3 = p3.construct(10)(Some(20))(Some(30))(List(1,2,3,4))
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
    ) should equal ((10, Some(20), Some(30), List(1, 2)))
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
      Some((10, Some(20), Some(30), List(1, 2))),
      None
    ))
  }

  test("Constructing locations from sites") {
    val s = r & r
    p.construct
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
    val z = site map ("000" + _)

    val l = z.construct.map(_(10)(20))
    l should equal (site.construct.map(_(10)(20)))

    val res = z.run(l.head)

    implicitly[res.type <:< String]

    res should equal ("00030")
  }

  test("map with various Path types") {
    val site2 = r & r2
    val z2 = site2.map("000" + _)
    val res2 = z2 run Location(List("add", "15", "plus", "20", "please"))
    val res3 = z2 run Location(List("addall", "10", "15", "20", "25"))
    implicitly[res2.type <:< String]
    implicitly[res3.type <:< String]

    res2 should equal ("00035")
    res3 should equal ("00070")
  }

  test("Can & together sitelets even if implicit resolution doesn't find a LUB") {
    val s1 = "a" :/: arg[Int] >> { i => i }
    val s2 = "b" :/: arg[String] >> { i => i }
    val s3 = "c" :/: arg[Boolean] >> { i => i }
//    implicitly[CanMapRoute[RouteType.PF[String with Int, RouteType.Const]]](CanMapRoute.pf)
    val s4_1 = s1 & s2
    val s4 = s1 & s2 & s3
    
    implicitly[s4.type <:< Sitelet[_, Any]]
  }

  test("Can & together sitelets, not only Sitelet&PathRoute") {
    val rt1 = "plus1" :/: arg[Int] >> (_ + 1)
    val rt2 = "plus2" :/: arg[Int] >> (_ + 2)
    val rt3 = "plus3" :/: arg[Int] >> (_ + 3)
    val s1 = rt2 & rt3
    val sitelet = rt1 & s1
    sitelet.run(Location("plus1" :: "10" :: Nil)) shouldBe 11
    val pathMapped = sitelet.mapPath("base" :/: _).by(identity)
    pathMapped.pathRoutes.head.path.construct(10) shouldBe Location("base" :: "plus1" :: "10" :: Nil)
  }

  test("Route can be a PartialFunction") {
    val rt = arg[Int] >>? { case i if i > 10 => i * 2 }
    rt.run.isDefinedAt(Location("10" :: Nil)) shouldBe false
    rt.run.isDefinedAt(Location("11" :: Nil)) shouldBe true
    rt.run.apply(Location("11" :: Nil)) shouldBe 22
  }

  test("Can mapPath by a PartialFunction") {
    val rt = arg[Int] >>? { case i if i > 10 => (i * 2).toDouble }
    val pathMapped = rt.mapPath(arg[String] :/: _) byPF (pf => { case str if str.length == 3 => pf })
    pathMapped.run.isDefinedAt(Location("aaa" :: "10" :: Nil)) shouldBe false
    pathMapped.run.isDefinedAt(Location("aaaa" :: "11" :: Nil)) shouldBe false
    pathMapped.run.isDefinedAt(Location("aaa" :: "11" :: Nil)) shouldBe true
    pathMapped.run.apply(Location("aaa" :: "11" :: Nil)) shouldBe 22
  }
}
