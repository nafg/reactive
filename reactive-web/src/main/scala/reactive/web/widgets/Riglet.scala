package reactive
package web
package widgets

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.{ ArrayBuffer, SynchronizedBuffer }
import scala.concurrent.{ ExecutionContext, Future }
import scala.xml.Elem

import reactive.{ Observing, Subscription }
import reactive.Applicative.ApType
import reactive.web.Page
import reactive.web.html.TextInput

import Applicative.ApType

class ErrorSourceId(val underlying: Int) extends AnyVal

object Id {
  val next: () => ErrorSourceId = {
    val current = new AtomicInteger(0)
    () => new ErrorSourceId(current.incrementAndGet())
  }
}

final case class ErrorMessage(message: String, source: ErrorSourceId)
object ErrorMessage {
  /** Create an error message associated with the given reader. */
  def create[A](message: String)(reader: Reader[A]): ErrorMessage = apply(message, reader.id)
}

sealed trait Result[+A] {
  def isSucess: Boolean = this match {
    case Result.Success(_) => true
    case Result.Failure(_) => false
  }
  def map[B](f: A => B): Result[B] = Result.map(f)(this)
}
object Result {
  case class Success[+A](a: A) extends Result[A]
  case class Failure(errors: List[ErrorMessage]) extends Result[Nothing]

  def failWith[A](message: String): Result[A] = Failure(List(ErrorMessage(message, new ErrorSourceId(0))))
  def ap[A, B](r1: Result[A => B], r2: Result[A]): Result[B] = (r1, r2) match {
    case (Success(f), Success(x))   => Success(f(x))
    case (Failure(m), Success(_))   => Failure(m)
    case (Success(_), Failure(m))   => Failure(m)
    case (Failure(m1), Failure(m2)) => Failure(m1 ++ m2)
  }
  def join[A](r: Result[Result[A]]): Result[A] = r match {
    case Failure(m)          => Failure(m)
    case Success(Failure(m)) => Failure(m)
    case Success(Success(x)) => Success(x)
  }
  def map[A, B](f: A => B)(ra: Result[A]): Result[B] = ra match {
    case Success(x) => Success(f(x))
    case Failure(m) => Failure(m)
  }
  def map2[A, B, C](f: A => B => C)(ra: Result[A])(rb: Result[B]): Result[C] = (ra, rb) match {
    case (Success(a), Success(b))   => Success(f(a)(b))
    case (Failure(ma), Failure(mb)) => Failure(ma ++ mb)
    case (Failure(m), _)            => Failure(m)
    case (_, Failure(m))            => Failure(m)
  }
  def iter[A](f: A => Unit) = (_: Result[A]) match {
    case Success(x) => f(x)
    case _          => ()
  }
  def bind[A, B](f: A => Result[B]): Result[A] => Result[B] = _ match {
    case Success(x) => f(x)
    case Failure(m) => Failure(m)
  }
}
import Result.{ Failure, Success }

abstract class Reader[+A](val id: ErrorSourceId) {
  def latest: Result[A]
  def subscribe(f: Result[A] => Unit): Subscription
  def subscribeImmediate(f: Result[A] => Unit): Subscription = {
    f(latest)
    subscribe(f)
  }
  def through[B](r: Reader[B]): Reader[A] = {
    val out = Stream(latest)
    r.subscribe {
      case Success(_) => out.trigger(latest)
      case Failure(msgs) =>
        (latest, msgs filter (_.source == id)) match {
          case (_, Nil)         => out.trigger(latest)
          case (Success(x), l)  => out.trigger(Failure(l))
          case (Failure(l), l2) => out.trigger(Failure(l ++ l2))
        }
    }
    out
  }
}

object Reader {
  def mapResult[A, B](f: Result[B] => Result[A])(r: Reader[B]): Reader[A] = {
    val out = Stream[A](f(r.latest))
    r.subscribe(out.trigger compose f)
    out
  }
  def mapResult2[A, B, C](f: Result[B] => Result[C] => Result[A])(rb: Reader[B])(rc: Reader[C]): Reader[A] = {
    val out = Stream[A](f(rb.latest)(rc.latest))
    rb.subscribe(b => out.trigger(f(b)(rc.latest)))
    rc.subscribe(c => out.trigger(f(rb.latest)(c)))
    out
  }
  def map[A, B](f: B => A)(r: Reader[B]): Reader[A] = mapResult[A, B](Result.map[B, A](f) _)(r)
  def map2[A, B, C](f: B => C => A)(rb: Reader[B])(rc: Reader[C]): Reader[A] =
    mapResult2[A, B, C](b => c => Result.map2[B, C, A](f)(b)(c))(rb)(rc)
  def mapToResult[A, B](f: B => Result[A])(r: Reader[B]): Reader[A] = mapResult[A, B](Result.bind(f))(r)
}
trait Writer[-A] {
  def trigger: Result[A] => Unit
}
final case class Stream[A](init: Result[A], override val id: ErrorSourceId = Id.next()) extends Reader[A](id) with Writer[A] {
  val s = Var[Result[A]](init)
  override def latest: Result[A] = s.now
  override def subscribe(f: Result[A] => Unit): Subscription = s subscribe f
  def trigger: Result[A] => Unit = s.update _
  /** Return a new Writer that sends x to this when triggered. */
  def write(x: A): Writer[Unit] = new ConcreteWriter[Unit]({
    case Failure(m)  => trigger(Failure(m))
    case Success(()) => trigger(Success(x))
  })
  def map[B](a2b: A => B)(b2a: B => A): Stream[B] = {
    val s2 = Stream[B](Result.map(a2b)(latest), id)
    val pa = new AtomicRef(latest)
    val pb = new AtomicRef(s2.latest)
    subscribe(a => if (pa.get != a) {
      pb.set(Result.map(a2b)(a))
      s2.trigger(pb.get)
    })
    s2.subscribe(b => if (pb.get != b) {
      pa.set(Result.map(b2a)(b))
      trigger(pa.get)
    })
    s2
  }
}
object Stream {
  def ap[A, B](sf: Stream[A => B])(sx: Stream[A]): Stream[B] = {
    val out = Stream(Result.ap(sf.latest, sx.latest))
    sf.subscribe(f => out.trigger(Result.ap(f, sx.latest)))
    sx.subscribe(x => out.trigger(Result.ap(sf.latest, x)))
    out
  }
  def apJoin[A, B](sf: Stream[A => B])(sx: Stream[Result[A]]): Stream[B] = {
    val out = Stream(Result.ap(sf.latest, Result.join(sx.latest)))
    sf.subscribe(f => out.trigger(Result.ap(f, Result.join(sx.latest))))
    sx.subscribe(x => out.trigger(Result.ap(sf.latest, Result.join(x))))
    out
  }
  def map[A, B](a2b: A => B)(b2a: B => A)(s: Stream[A]): Stream[B] = s.map(a2b)(b2a)
}
final class ConcreteWriter[A](val trigger: Result[A] => Unit) extends Writer[A]
object ConcreteWriter {
  def apply[A](trigger: A => Unit) = new ConcreteWriter[A]({
    case Success(x) => trigger(x)
    case Failure(_) => ()
  })
}
final class ConcreteReader[A](val latest: Result[A], subscribeFn: (Result[A] => Unit) => Subscription) extends Reader[A](Id.next()) {
  def subscribe(f: Result[A] => Unit): Subscription = subscribe(f)
}
final class Submitter[A](val input: Reader[A], clearError: Boolean) extends Reader[A](Id.next()) with Writer[Unit] {
  val output = Stream[A](Failure(Nil))
  val writer: Writer[Unit] = new ConcreteWriter[Unit](UnitIn => (UnitIn, input.latest) match {
    case (Failure(m1), Failure(m2)) => output.trigger(Failure(m1 ++ m2))
    case (Failure(m), Success(_))   => output.trigger(Failure(m))
    case (Success(_), Failure(m))   => output.trigger(Failure(m))
    case (Success(()), Success(x))  => output.trigger(Success(x))
  })
  if (clearError)
    input.subscribe(_ => output.trigger(Failure(Nil)))
  def trigger = _ => writer.trigger(Success(()))
  def latest: Result[A] = output.latest
  def subscribe(f: Result[A] => Unit): Subscription = output.subscribe(f)
}

object Pervasives {
  implicit class Ext_<<^[A, B, C](v: A => B => C) {
    /**
     * Push an argument to the view function
     */
    def <<^(a: B): A => C = x => v(x)(a)
  }
  implicit class Ext_>>^[A, B](v: A => B) {
    /**
     * Map argument(s) to the view function
     */
    def >>^(f: A): Yielder[B] = SimpleYielder[B](v(f))
  }
  implicit class Ext_<*>[A, B, C, D](f: Piglet[A => B, C => D]) {
    /** Apply a Piglet function to a Piglet value */
    def <*>[E](x: Piglet[A, D => E]): Piglet[B, C => E] = Piglet.ap(f)(x)
  }
  implicit class Ext_<*?>[A, B, C, D](f: Piglet[A => B, C => D]) {
    /** Apply a Piglet function to a Piglet Result */
    def <*?>[E](x: Piglet[Result[A], D => E]): Piglet[B, C => E] = Piglet(Stream.apJoin(f.stream)(x.stream), f.view andThen x.view)
  }

  implicit class Ext_|>[A](a: A) {
    def |>[B](f: A => B): B = f(a)
  }

  implicit class toPigletOps[A](a: A) {
    def toP = Piglet.`yield`[A](a)
  }

  import Applicative.ApType

  trait PigletApBase[V1, VLast, AT <: ApType] {
    def ap[Z, V0](f: Piglet[AT#FS[Z], V0 => V1]): Piglet[Z, V0 => VLast]
    def apRet[Z](f: AT#FS[Z]): Piglet[Z, V1 => VLast] = ap[Z, V1](Piglet.`return`[AT#FS[Z], V1](f))
    def :@:[B, V0](p: Piglet[B, V0 => V1]): PigletApBase[V0, VLast, ApType.More[B, AT]] =
      new PigletApMore[B, V0, V1, VLast, AT](p, this)
  }
  implicit class PigletApOne[A, V1, VLast](val in: Piglet[A, V1 => VLast]) extends PigletApBase[V1, VLast, ApType.One[A]] {
    override def ap[Z, V0](f: Piglet[A => Z, V0 => V1]): Piglet[Z, V0 => VLast] = Piglet.ap(f)(in)
  }
  class PigletApMore[A, V1, V2, VLast, N <: ApType](val p: Piglet[A, V1 => V2], val next: PigletApBase[V2, VLast, N]) extends PigletApBase[V1, VLast, ApType.More[A, N]] {
    override def ap[Z, V0](f: Piglet[A => N#FS[Z], V0 => V1]): Piglet[Z, V0 => VLast] = next.ap(Piglet.ap(f)(p))
  }
}

/**
 * @param stream the stream associated with the Piglet.
 */
case class Piglet[A, V](stream: Stream[A], view: V) extends PigletValidationMethods[A, V] {
  def mapView[V2](f: V => V2): Piglet[A, V2] = this.copy(view = f(view))
  def map[B](f: A => B) = Piglet.map(f) apply this

}

trait PigletValidationMethods[A, V] { this: Piglet[A, V] =>
  def validate(msg: String)(pred: A => Boolean): Piglet[A, V] =
    validate(ErrorMessage(msg, stream.id))(pred)
  def validate(msg: ErrorMessage)(pred: A => Boolean): Piglet[A, V] = {
    val s = Stream(stream.latest, stream.id)
    stream.subscribe {
      case Failure(m)            => s.trigger(Failure(m))
      case Success(x) if pred(x) => s.trigger(Success(x))
      case _                     => s.trigger(Failure(List(msg)))
    }
    copy(stream = s)
  }
}

// (f: A => B) => f(a) : (A => B) => B
// (f: A => ?) => f(a) : (A => ?) => ?

trait Yielder[A] {
  def apply[B](f: A => B): B
}
case class SimpleYielder[A](a: A) extends Yielder[A] {
  def apply[B](f: A => B) = f(a)
}

object Piglet {
  import Pervasives._

  def ap[A, B, C, D, E](f: Piglet[A => B, C => D])(x: Piglet[A, D => E]): Piglet[B, C => E] =
    Piglet(Stream.ap(f.stream)(x.stream), f.view andThen x.view)

  /** Create a Piglet from a stream and a view. */
  def create[A, V](s: Stream[A])(v: V): Piglet[A, V] = Piglet(s, v)

  /** Map the arguments passed to the view. */
  def mapViewArgs[A, VA, VB, VC](p: Piglet[A, VA => VB])(view: VA): Piglet[A, (VB => VC) => VC] =
    Piglet(p.stream, p.view >>^ view)

  /** Create a Piglet initialized with x that passes its stream to the view */
  def `yield`[A](x: A): Piglet[A, Yielder[Stream[A]]] = {
    val s = Stream(Success(x))
    Piglet(s, SimpleYielder(s))
  }
  /** Create a Piglet initialized with failure that passes its stream to the view */
  def yieldFailure[A](): Piglet[A, Yielder[Stream[A]]] = {
    val s = Stream[A](Failure(Nil))
    Piglet(s, SimpleYielder(s))
  }
  /**
   * Create a Piglet with optional value initialized with init
   * that passes its stream to the view.
   * The stream passed is a non-optional stream,
   * and the given noneValue is mapped to None.
   */
  def yieldOption[A, B](x: Option[A])(none: A) = {
    val p = `yield`(x).mapView(_.apply[Stream[A]] _)
    val v = (_: Stream[Option[A]]).map[A](_ getOrElse none)((Some(_) filter (_ != none)))
    mapViewArgs(p)(v)
  }
  /** Create a Piglet initialized with x that doesn't pass any stream to the view */
  def `return`[A, B](x: A): Piglet[A, B => B] = Piglet(Stream(Success(x)), identity[B] _)
  /** Create a Piglet initialized with x that doesn't pass any stream to the view. */
  def returnFailure[A, B](): Piglet[A, B => B] = Piglet(Stream[A](Failure(Nil)), identity[B] _)
  /**
   * Create a Piglet value that streams the value every time it receives a signal.
   * The signaling function is passed to the view.
   */
  def withSubmit[A, B, C](pin: Piglet[A, B => Submitter[A] => C]): Piglet[A, B => C] = {
    val submitter = new Submitter(pin.stream, clearError = false)
    Piglet(submitter.output, pin.view <<^ submitter)
  }

  /** Piglet that returns many values with an additional piglet used to create new values in the collection */
  def manyPiglet[A, V, W, X, Y, Z](inits: Seq[A])(create: (Piglet[A, Y => Z]))(p: (A => Piglet[A, V => W])): Piglet[Seq[A], (Many.Stream[A, V, W, Y, Z] => X) => X] = {
    val s = Stream(Success(inits))
    val m = new Many.Stream[A, V, W, Y, Z](p, s, create)
    Piglet(s, f => f(m))
  }

  /** Create a Piglet that returns many values, each created according to the given Piglet. */
  def many[A, V, W, X](init: A)(p: (A => Piglet[A, V => W])): Piglet[Seq[A], (Many.UnitStream[A, V, W] => X) => X] = manyInit(List(init))(init)(p)

  /** Create a Piglet that returns many values, each created according to the given Piglet. */
  def manyInit[A, V, W, X](inits: Seq[A])(init: A)(p: (A => Piglet[A, V => W])): Piglet[Seq[A], (Many.UnitStream[A, V, W] => X) => X] = {
    val s = Stream(Success(inits))
    val _init = p(init)
    val m = new Many.UnitStream[A, V, W](p, s, _init, init)
    Piglet(s, _(m))
  }

  /** Create a Piglet that allows the user to choose between several options. */
  def choose[I, O, U, V, W, X, Y](chooser: Piglet[I, U => V])(choices: (I => Piglet[O, W => X])): Piglet[O, (Choose.Stream[O, I, U, V, W, X] => Y) => Y] = {
    val s = Stream[O](Failure(Nil))
    val c = new Choose.Stream[O, I, U, V, W, X](chooser, choices, s)
    Piglet(s, f => f(c))
  }

  /** Create a Piglet value that streams the value every time it receives a signal. */
  /** The signaling function is passed to the view. */
  /** Any update to the input Piglet passes `Failure []` to the output. */
  /** This is useful to clear error messages from a previous submission. */
  def withSubmitClearError[A, B, C](pin: Piglet[A, B => Submitter[A] => C]): Piglet[A, B => C] = {
    val submitter = new Submitter(pin.stream, clearError = false)
    Piglet(submitter.output, pin.view <<^ submitter)
  }

  /** Pass this Piglet's stream to the view. */
  def transmitStream[A, B, C]: Piglet[A, B => Stream[A] => C] => Piglet[A, B => C] =
    p => Piglet(p.stream, p.view <<^ p.stream)

  /** Pass a reader for this Piglet's stream to the view. */
  def transmitReader[A, B, C]: Piglet[A, B => Reader[A] => C] => Piglet[A, B => C] =
    p => Piglet(p.stream, p.view <<^ p.stream)

  /** Pass a mapped reader for this Piglet's stream to the view. */
  def transmitReaderMap[A, B, C, D](f: A => D)(p: Piglet[A, B => Reader[D] => C]): Piglet[A, B => C] =
    Piglet(p.stream, p.view <<^ Reader.map(f)(p.stream))

  /** Pass a mapped reader for this Piglet's stream to the view. */
  def transmitReaderMapResult[A, B, C, D](f: Result[A] => Result[D])(p: Piglet[A, B => Reader[D] => C]): Piglet[A, B => C] =
    Piglet(p.stream, p.view <<^ Reader.mapResult(f)(p.stream))

  /** Pass a mapped reader for this Piglet's stream to the view. */
  def transmitReaderMapToResult[A, B, C, D](f: A => Result[D])(p: Piglet[A, B => Reader[D] => C]): Piglet[A, B => C] =
    Piglet(p.stream, p.view <<^ Reader.mapToResult(f)(p.stream))

  /** Pass a writer for this Piglet's stream to the view. */
  def transmitWriter[A, B, C]: Piglet[A, B => Writer[A] => C] => Piglet[A, B => C] =
    p => Piglet(p.stream, p.view <<^ p.stream)

  /** Map the value of a Piglet, without changing its view. */
  def map[A, B, V](m: A => B): Piglet[A, V] => Piglet[B, V] =
    mapResult {
      case Failure(msg) => Failure(msg)
      case Success(x)   => Success(m(x))
    }

  /** Map the value of a Piglet, without changing its view. */
  def mapToResult[A, B, V](m: A => Result[B]): Piglet[A, V] => Piglet[B, V] =
    mapResult {
      case Failure(msg) => Failure(msg)
      case Success(x)   => m(x)
    }

  /** Map the Result of a Piglet, without changing its view. */
  def mapResult[A, B, V](m: Result[A] => Result[B])(p: Piglet[A, V]): Piglet[B, V] = {
    val out = Stream(m(p.stream.latest))
    p.stream.subscribe(out.trigger compose m)
    Piglet(out, p.view)
  }

  /** Map the value of a Piglet, without changing its view. */
  def mapAsync[A, B, V](m: A => Future[B])(implicit ec: ExecutionContext): Piglet[A, V] => Piglet[B, V] =
    mapAsyncResult {
      case Failure(msg) => Future.successful(Failure(msg))
      case Success(x)   => m(x).map(res => Success(res))
    }

  /** Map the value of a Piglet, without changing its view. */
  def mapToAsyncResult[A, B, V](m: A => Future[Result[B]])(implicit ec: ExecutionContext): Piglet[A, V] => Piglet[B, V] =
    mapAsyncResult {
      case Failure(msg) => Future.successful(Failure(msg))
      case Success(x)   => m(x)
    }

  /** Map the Result of a Piglet, without changing its view. */
  def mapAsyncResult[A, B, V](m: Result[A] => Future[Result[B]])(p: Piglet[A, V])(implicit ec: ExecutionContext): Piglet[B, V] = {
    val out = Stream[B](Failure(Nil))
    p.stream.subscribe { v =>
      m(v) foreach out.trigger
    }
    m(p.stream.latest) foreach out.trigger
    Piglet(out, p.view)
  }

  /** Map the value of a Piglet, without changing its view. */
  /** The function can write directly into the output, zero, one or many times. */
  def mapWithWriter[A, B, V](f: Writer[B] => A => Unit)(p: Piglet[A, V]): Piglet[B, V] = {
    def f2(out: Writer[B])(r: Result[A]) = r match {
      case Failure(msgs) => out.trigger(Failure(msgs))
      case Success(x)    => f(out)(x)
    }
    mapResultWithWriter(f2)(p)
  }

  /** Map the Result of a Piglet, without changing its view. */
  /** The function can write directly into the output, zero, one or many times. */
  def mapResultWithWriter[A, B, V](f: Writer[B] => Result[A] => Unit)(p: Piglet[A, V]): Piglet[B, V] = {
    val stream = Stream[B](Failure(Nil))
    p.stream.subscribeImmediate(f(stream))
    Piglet(stream, p.view)
  }

  /** Flush error messages, replacing any failing state with a message-less failing state. */
  def flushErrors[A, V]: Piglet[A, V] => Piglet[A, V] =
    mapResult {
      case Failure(_) => Failure(Nil)
      case x          => x
    }

  /** Run the action every time the Piglet's stream receives successful data. */
  def run[A, B](action: (A => Unit))(p: Piglet[A, B]): Piglet[A, B] =
    runResult(Result.iter(action))(p)

  /** Run the action every time the Piglet's stream receives data. */
  def runResult[A, B](action: (Result[A] => Unit))(p: Piglet[A, B]): Piglet[A, B] = {
    p.stream subscribe action
    p
  }

  /** Run a Piglet UI with the given view. */
  def render[A, V, Elt]: V => Piglet[A, V => Elt] => Elt =
    view => p => p.view(view)

  /**
   *  Create a Piglet for a double field for confirmation (e.g. for passwords).
   *  @tparam A the piglet value type
   *  @param init the initial value
   *  @param validate a function that transforms a Piglet's view type from `(Stream[A]=>B)=>B` to `(C=>D=>(C,D)) => Stream[A] => E`
   *  @param nomatch the error message for when the two don't match
   */
  def confirm[A, B, C, D, E, F](init: A)(validate: Piglet[A, (Stream[A] => B) => B] => Piglet[A, (C => D => (C, D)) => Stream[A] => E])(nomatch: String): Piglet[A, ((E => F) => F)] = {
    val first: Piglet[A, (Stream[A] => B) => B] = `yield`(init)
    val second: Piglet[A, (Stream[A] => E) => E] = `yield`(init)
    val x1 = validate(first) :@: second apRet { a => b => (a, b) }
    val x2 = x1.validate(ErrorMessage.create(nomatch)(second.stream)) { case (a, b) => a == b }
    mapViewArgs(x2.map(_._1)) { a => b => (a, b) }
  }

  abstract class Builder {
    def bind[I, O, U, V, W, X, Y](p: Piglet[I, U => V], f: (I => Piglet[O, W => X])): Piglet[O, (Choose.Stream[O, I, U, V, W, X] => Y) => Y] =
      choose(p)(f)

    def `return`[A, B]: A => Piglet[A, B => B] = Piglet.`return`

    def returnFrom[A, V]: Piglet[A, V] => Piglet[A, V] = identity

    def `yield`[A]: A => Piglet[A, Yielder[Stream[A]]] = Piglet.`yield`[A]

    def yieldFrom[A, V]: Piglet[A, V] => Piglet[A, V] = identity

    def zero[A, B](): Piglet[A, B => B] = returnFailure()
  }
  object Do extends Builder

}

trait Container[In, Out] {
  def add: In => Unit
  def remove: Int => Unit
  def moveUp: Int => Unit
  def container: Out
}

object Many {
  class Operations(deleteFn: () => Unit, val moveUp: Submitter[Unit], val moveDown: Submitter[Unit]) {
    def delete: Writer[Unit] = ConcreteWriter(_ => deleteFn())
  }

  class Stream[A, V, W, Y, Z](p: A => Piglet[A, V => W], out: widgets.Stream[Seq[A]], adder: Piglet[A, Y => Z]) extends Reader[Seq[A]](out.id) {
    var streams = Vector.empty[widgets.Stream[A]]
    def update() = {
      val res = streams.foldLeft(Success(Nil): Result[List[A]]) {
        case (acc, cur) => (acc, cur.latest) match {
          case (Success(l), Success(x))   => Success(x :: l)
          case (Failure(m), Success(_))   => Failure(m)
          case (Success(_), Failure(m))   => Failure(m)
          case (Failure(m1), Failure(m2)) => Failure(m1 ++ m2)
        }
      }.map(_.reverse: Seq[A])
      out.trigger(res)
    }
    def subscribe(f: Result[Seq[A]] => Unit): Subscription = out.subscribe(f)
    def latest = out.latest
    /**
     * Render the element collection inside this Piglet
     * inside the given container and with the provided
     * rendering function
     */
    def render[U](c: Container[W, U])(f: Operations => V): U = {
      def add(x: A) = {
        val piglet = p(x)
        streams :+= piglet.stream
        piglet.stream.subscribeImmediate(_ => update())
        def getThisIndex = streams.indexWhere(_.id == piglet.stream.id)
        def moveBy(i: Int) = synchronized {
          if (i > 0 && i < streams.length) {
            streams = streams.patch(i - 1, streams.slice(i - 1, i), 2)
            c.moveUp(i)
            update()
          }
        }
        def moveDown() = moveBy(getThisIndex + 1)
        def moveUp() = moveBy(getThisIndex)
        def canMoveUp = if (getThisIndex > 0) Success(()) else Failure(Nil)
        def canMoveDown = if (getThisIndex < streams.length) Success(()) else Failure(Nil)
        val inMoveUp = Stream(canMoveUp)
        val inMoveDown = Stream(canMoveDown)
        val outSubscription = out.subscribe(_ => {
          inMoveUp.trigger(canMoveUp)
          inMoveDown.trigger(canMoveDown)
        })
        val subMoveUp = new Submitter(inMoveUp, false)
        val subMoveDown = new Submitter(inMoveDown, false)
        val subUpSubscription = subMoveUp.subscribe(Result.iter(_ => moveUp()))
        val subDownSubscription = subMoveDown.subscribe(Result.iter(_ => moveDown()))
        def delete() = {
          val i = synchronized {
            val i = getThisIndex
            streams = streams.patch(i, Nil, 1)
            i
          }
          c.remove(i)
          outSubscription.unsubscribe()
          subUpSubscription.unsubscribe()
          subDownSubscription.unsubscribe()
          update()
        }
        c.add(piglet.view(f(new Operations(delete, subMoveUp, subMoveDown))))
      }
      out.latest match {
        case Failure(_)  => ()
        case Success(xs) => xs foreach add
      }
      adder.stream.subscribe {
        case Failure(_)    => ()
        case Success(init) => add(init)
      }
      c.container
    }

    /** Stream where new elements for the collection are written */
    def addA: Writer[A] = adder.stream
    /** Function that provides the Adder Piglet with a rendering function */
    def addRender(f: Y): Z = adder.view(f)
  }

  class UnitStream[A, V, W](p: A => Piglet[A, V => W], out: widgets.Stream[Seq[A]], init: Piglet[A, V => W], default: A) extends Stream[A, V, W, V, W](p, out, init) {
    val submitStream = {
      val submitter = Stream[Unit](Failure(Nil))
      val trigger = init.stream.trigger
      submitter.subscribe {
        case Failure(msgs) => trigger(Failure(msgs))
        case Success(())   => trigger(Success(default))
      }
      submitter
    }
    /** Add an element to the collection set to the default values */
    def addU: Writer[Unit] = submitStream
  }
}
object Choose {
  class Stream[O, I, U, V, W, X](chooser: Piglet[I, U => V], choice: I => Piglet[O, W => X], out: widgets.Stream[O]) extends Reader[O](out.id) with Subscription {
    val pStream = Stream[(I, Piglet[O, W => X])](Failure(Nil))
    val choiceSubscriptions = collection.mutable.Map[I, (Piglet[O, W => X], Subscription)]()
    val subscriptions = new AtomicRef(
      List(
        chooser.stream.subscribeImmediate(res => pStream.trigger(
          res.map(i =>
            (i, if (choiceSubscriptions.keySet.contains(i)) choiceSubscriptions(i)._1 else {
              val p = choice(i)
              choiceSubscriptions(i) = (p, p.stream.subscribe(out.trigger))
              p
            })
          )
        ))
      )
    )
    override def latest = out.latest
    override def subscribe(f: Result[O] => Unit): Subscription = out.subscribe(f)
    /** Render the Piglet that allows the user to choose between different options. */
    def chooser(f: U): V = chooser.view(f)
    /** Get the stream of the Piglet that allows the user to choose between different options */
    def chooserStream: widgets.Stream[I] = chooser.stream
    /** Render the Piglet that allows the user to choose the value for the selected option */
    def choice[Y](c: Container[X, Y])(f: W): Y = {
      val renders = Map[I, Nothing]()
      val hasChild = new AtomicRef(false)
      subscriptions.transform(
        pStream.subscribeImmediate(res => res match {
          case Failure(_) => ()
          case Success((i, p)) =>
            val render = if (renders.keySet contains i) renders(i)
            else p.view(f)
            out.trigger(p.stream.latest)
            if (!hasChild.get()) c.remove(0)
            hasChild.set(true)
            c.add(render)
        }) :: _
      )
      c.container
    }
    def cleanUp(): Unit = {
      subscriptions.get.foreach(_.unsubscribe())
      choiceSubscriptions foreach {
        case (_, (_, s)) => s.unsubscribe()
      }
    }
  }
}

trait Controls {
  import scala.xml.Elem

  trait IPagelet

  val nextId = {
    val current = new AtomicInteger(0)
    () => "pl__" + current.incrementAndGet()
  }

  def _input[A](implicit observing: Observing, page: Page) = {
    `type`: String =>
      ofString: (String => A) => toString: (A => String) => (stream: Stream[A]) =>
        val i = new TextInput {
          override def baseElem = <input type={ `type` } />
        }
        stream.latest match {
          case Failure(_) => ()
          case Success(x) => i.value() = toString(x)
        }
        stream.subscribe {
          case Success(x) =>
            val s = toString(x)
            if (i.value.now != s) i.value() = s
          case Failure(_) => ()
        }
        def ev() = {
          val v = Success(ofString(i.value.now))
          if (v != stream.latest) stream.trigger(v)
        }
        i.keyUp.eventStream.foreach(_ => ev())
        i.change.eventStream.foreach(_ => ev())
        i
  }

  /** A label for a given element. */
  def withLabel: String => Elem => Elem

  /** A label for a given element. */
  def withLabelAfter: String => Elem => Elem

  /** A Piglet text input. */
  def input: Stream[String] => Elem

  /** A Piglet password input. */
  def password: Stream[String] => Elem

  /** A Piglet text area. */
  def textArea: Stream[String] => Elem

  /** A Piglet text input that accepts integers. */
  def intInput: Stream[Int] => Elem

  /** A Piglet checkbox. */
  def checkBox: Stream[Boolean] => Elem

  /** A Piglet radio button set. */
  def radio[A]: Stream[A] => Seq[(A, String)] => Elem

  /** A Piglet combobox. */
  def select[A]: Stream[A] => Seq[(A, String)] => Elem

  /** Render a multiple-valued stream. */
  def renderMany[A, V](stream: Many.Stream[A, V, Elem, _, _])(renderOne: (Many.Operations => V))(container: Elem): Elem

  /** Render a choice stream. */
  def renderChoice[O, I, U, V, W](stream: Choose.Stream[O, I, U, V, W, Elem])(renderOne: W)(container: Elem): Elem

  /** Display a reactive value. */
  def showResult[A](reader: Reader[A])(render: (Result[A] => Seq[IPagelet]))(container: Elem): Elem

  /** Display a reactive value, or nothing if it is invalid. */
  def show[A](reader: Reader[A])(render: A => Seq[IPagelet])(container: Elem): Elem

  /** Display a reactive value, or nothing if it is invalid. */
  def showString[A](reader: Reader[A])(render: (A => String))(container: Elem): Elem

  /** Display errors, if any. */
  def showErrors[A](reader: Reader[A])(render: (List[String] => Seq[IPagelet]))(container: Elem): Elem

  /** Add an attribute to an element that depends on a reader. */
  def attr[A](reader: Reader[A])(attrName: String)(render: (A => String)): Elem => Elem

  /** Add an attribute to an element that depends on a reader. */
  def attrResult[A](reader: Reader[A])(attrName: String)(render: (Result[A] => String)): Elem => Elem

  /** Add a CSS style to an element that depends on a reader. */
  def css[A](reader: Reader[A])(attrName: String)(render: (A => String)): Elem => Elem

  /** Add a CSS style to an element that depends on a reader. */
  def cssResult[A](reader: Reader[A])(attrName: String)(render: (Result[A] => String)): Elem => Elem

  /** Displays a submit button driven by the given submitter. */
  def submit: Writer[Unit] => Elem

  /** A button that triggers the given callback. */
  def button: Writer[Unit] => Elem

  /** Displays a submit button driven by the given submitter. */
  /** The button is disabled when no input is available. */
  def submitValidate[A]: Submitter[A] => Elem

  /** A button that triggers the given callback. */
  /** The button is disabled when no input is available. */
  def buttonValidate[A]: Submitter[A] => Elem

  /** An <a> link that triggers the given callback. */
  def link: Writer[Unit] => Elem

  /** Enables the element when reading Success, disable it when reading Failure. */
  def enableOnSuccess[A]: Reader[A] => Elem => Elem
}

object TestPiglets {
  import Pervasives._

  sealed trait Species; object Species {
    object Cat extends Species; object Dog extends Species; object Piglet extends Species
    def showSpecies = (_: Species) match {
      case Cat    => "cat"
      case Dog    => "dog"
      case Piglet => "piglet"
    }
  }
  case class Pet(species: Species, name: String)
  case class Person(first: String, last: String, pets: Seq[Pet])
  val dictionary = Set(("Alonzo", "Church"), "Alan" -> "Turing", "Edsger" -> "Dijkstra", "Charles" -> "Babbage")
  val defaultPet = Pet(species = Species.Piglet, name = "Spot")

  def petPiglet(init: Pet) = {
    val pn = init.name.toP.validate("Please enter the pet's name")(_ != "")
    val ps = init.species.toP
    ps :@: pn apRet { s: Species => n: String => Pet(s, n) }
  }

  def personPiglet(init: Person) = {
    val pf = init.first.toP.validate("Please enter a first name")(_ != "")
    val pl = init.last.toP.validate("Please enter a last name")(_ != "")
    val pp =
      Piglet.many[Pet, Stream[Species] => (Stream[String] => Stream[String]), Stream[String], Int](defaultPet)(petPiglet)
    val pp1 = PigletApOne(pp).:@:(pl)
    // val pp2 = (pf :@: pl :@: pp) apRet { first => last => pets => Person(first, last, pets)}
    // val pv = pp2.validate("Unknown user")(dictionary.contains)
  }

  /*
 0 let PersonPiglet (init: Person) =
31 Return (fun first last pets →
32 { first = first;
33 last = last;
34 pets = pets })
35 ⊗ (Yield init.first
36 |> Validation.Is Validation.NotEmpty
37 "Please enter a first name.")
38 ⊗ (Yield init.last
39 |> Validation.Is Validation.NotEmpty
40 "Please enter a last name.")
41 ⊗ Many defaultPet PetPiglet
42 |> Validation.Is (fun fullName →
43 dictionary.Contains
44 (fullName.first, fullName.last))
45 "Unknown user."
46 |> WithSubmit
47
48 let initUser =
49 {first = "Alonzo"; last = "Church"; pets = [||]}
   */
}
