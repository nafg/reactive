package reactive.routing

import scala.annotation.{compileTimeOnly, implicitNotFound}

/**
 * Given a [[Call]] and a raw [[Request]]
 * materialize a raw [[Response]] somehow
 * (perhaps via HTTP)
 */
trait Client[Request, Response] {
  def run(call: Call, updateRequest: Request => Request): Response
}

/**
 * Typeclass to expose a handler
 * as a web-framwork-specific `Handler`
 * @tparam A the handler type
 */
trait RequestReceiver[A] {
  type Handler

  def wrap(f: A): Handler
}

case class Operation[R <: RouteType, A](path: Path[R], verb: Option[HttpVerb] = None) {
  def run(route: R#Route[A]): PartialFunction[Call, A] = Function.unlift { call =>
    if (verb.forall(_ == call.verb)) path.run(route).lift(call.loc)
    else None
  }
}

object Operation {

  implicit class OperationOps[R <: RouteType, A, V <: HttpVerb](self: Operation[R, HttpOp[V, A]])(implicit verb: V, lift: AndThen[R#Func]) {
    def construct: R#Func[CallWithType[A]] = lift[Location, CallWithType[A]](loc => new Call(loc, verb) with CallWithType[A])(self.path.construct)
  }

  implicit class OperationHttpOps[R <: RouteType, A, V <: HttpVerb](o: Operation[R, HttpOp[V, A]])(implicit verb: V) {
    def >>?(rte: R#Route[HttpOp[V, A]])(implicit mapRoute: AndThen[R#Route], rr: RequestReceiver[A]): OpRoute[R, rr.Handler] =
      new OpRoute[R, rr.Handler](o.copy(verb = Some(verb)), mapRoute[HttpOp[V, A], rr.Handler](o => rr.wrap(o.handler))(rte))

    def >>(rte: R#Func[HttpOp[V, A]])(implicit mapRoute: AndThen[R#Route], lift: FnToPF[R], rr: RequestReceiver[A]): OpRoute[R, rr.Handler] =
      new OpRoute[R, rr.Handler](o.copy(verb = Some(verb)), mapRoute[HttpOp[V, A], rr.Handler](o => rr.wrap(o.handler))(lift(rte)))
  }

}

sealed abstract class HttpVerb {
  def method = getClass.getSimpleName.stripSuffix("$")
}

case object HttpVerb {

  implicit case object GET extends HttpVerb

  implicit case object PUT extends HttpVerb

  implicit case object POST extends HttpVerb

  implicit case object DELETE extends HttpVerb

  implicit case object PATCH extends HttpVerb

  implicit case object HEAD extends HttpVerb

  implicit case object OPTIONS extends HttpVerb

  val verbs = Seq(GET, PUT, POST, DELETE, PATCH, HEAD, OPTIONS)

  implicit class toOp[V <: HttpVerb](self: V) {
    def apply[A](a: A) = new HttpOp[V, A] {
      val verb = self
      val handler = a
    }
  }

}

sealed trait HttpOp[V, +A] {
  def verb: V

  def handler: A
}

case class Call(loc: Location, verb: HttpVerb = HttpVerb.GET)

trait CallWithType[A] extends Call

@implicitNotFound("Could not unpack ${A} as ${I} => ${O}. Ensure you have an appropriate instance of HandlerUnpacker in your implicit scope.")
final class HandlerUnpacker[A, I, O]

object HandlerUnpacker {
  implicit def function[I, O]: HandlerUnpacker[I => O, I, O] = new HandlerUnpacker[I => O, I, O]
}

trait MessageConverter[A, B] {
  def apply(a: A): B
}

trait CallLow {
  implicit class errorFallback[A](call: CallWithType[A]) {
    @compileTimeOnly("Cannot call run on a CallWithType[A] unless there is an implicit HandlerUnpacker for it. Is A a function type?")
    def run[Z, ZZ](z: Any) = sys.error("Cannot call run on a CallWithType[A] unless there is an implicit HandlerUnpacker for it. Is A a function type?")
  }
}
object Call extends CallLow {
  implicit class CallOps[A, I, O](call: CallWithType[A])(implicit val unpack: HandlerUnpacker[A, I, O]) {
    def run[Req, Res](in: I)(
      implicit client: Client[Req, Res],
      convertRequest: MessageConverter[I, Req => Req],
      convertResponse: MessageConverter[Res, O]
      ): O =
      convertResponse(client.run(call, convertRequest(in)))
  }
}

trait AsHeader[A] {
  def pickle(value: A): Seq[(String, String)]

  def unpickle(headers: Seq[(String, String)]): Option[A]
}
