import reactive.routing.HttpVerb.PUT
import reactive.routing._


sealed trait Pickler[Native, Pickled] {
  def pickle(in: Native): Pickled
  def unpickle(request: Pickled): Native
}

object Test extends App {
  implicit def receiver[In, Out, V <: HttpVerb, O <: HttpOp[V, In => Out]](implicit requestPickler: Pickler[In, String], responsePickler: Pickler[Out, String]) =
    new RequestReceiver[In => Out] {
      type Handler = String => String
      override def wrap(f: (In) => Out): (String) => String = request => responsePickler.pickle(f(requestPickler.unpickle(request)))
    }

  case class Person(name: String)
  implicit val personConverter: Pickler[Person, String] = new Pickler[Person, String] {
    def pickle(a: Person) = a.name
    def unpickle(a: String) = Person(a)
  }
  implicit val personReqConverter: MessageConverter[Person, String => String] = new MessageConverter[Person, String => String] {
    def apply(a: Person) = _ => a.name
  }
  implicit val intPickler: Pickler[Int, String] = new Pickler[Int, String] {
    override def pickle(in: Int) = in.toString
    override def unpickle(request: String) = request.toInt
  }
  implicit val intResConverter: MessageConverter[String, Int] = new MessageConverter[String, Int] {
    override def apply(a: String): Int = a.toInt
  }

  def p = "a" :/: arg[Int]

  val op: Operation[Int >>: RConst, HttpOp[PUT.type, (Person) => Int]] = p.toOp[Person => Int](PUT)
  val route = op.>>(i => PUT(_.name.length + i * 100))

  implicit object client extends Client[String, String] {
    override def run(call: Call, request: String => String) = route.run(call)(request(""))
  }

  val c = op.construct
  println(client.run(Call(Location("a" :: "9" :: Nil), PUT), _ => "joe"))
  println(c(9).run(Person("joe")))
}
