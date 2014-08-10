package reactive

package web.widgets
import Result.{ Failure, Success }
import java.util.concurrent.atomic.AtomicInteger

class ErrorSourceId(val underlying: Int) extends AnyVal

object Ids {
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
final case class Stream[A](init: Result[A], override val id: ErrorSourceId = Ids.next()) extends Reader[A](id) with Writer[A] {
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
  def ap[B](that: Stream[A => B]): Stream[B] = {
    val out = Stream(Result.ap(that.latest, this.latest))
    that.subscribe(f => out.trigger(Result.ap(f, this.latest)))
    this.subscribe(x => out.trigger(Result.ap(that.latest, x)))
    out
  }
}
object Stream {
  def apJoin[A, B](sf: Stream[A => B])(sx: Stream[Result[A]]): Stream[B] = {
    val out = Stream(Result.ap(sf.latest, Result.join(sx.latest)))
    sf.subscribe(f => out.trigger(Result.ap(f, Result.join(sx.latest))))
    sx.subscribe(x => out.trigger(Result.ap(sf.latest, Result.join(x))))
    out
  }
}
final class ConcreteWriter[A](val trigger: Result[A] => Unit) extends Writer[A]
object ConcreteWriter {
  def apply[A](trigger: A => Unit) = new ConcreteWriter[A]({
    case Success(x) => trigger(x)
    case Failure(_) => ()
  })
}
final class ConcreteReader[A](val latest: Result[A], subscribeFn: (Result[A] => Unit) => Subscription) extends Reader[A](Ids.next()) {
  def subscribe(f: Result[A] => Unit): Subscription = subscribe(f)
}
final class Submitter[A](val input: Reader[A], clearError: Boolean) extends Reader[A](Ids.next()) with Writer[Unit] {
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
