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

import Result.{ Failure, Success }

import Applicative.ApType

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
  def toFn[B]: (A => B) => B = apply[B] _
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
  def mapViewArgs[A, VA, VB](p: Piglet[A, VA => VB])(view: VA): Piglet[A, Yielder[VB]] =
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
  def many[A, V, W](init: A)(p: (A => Piglet[A, V => W])): Piglet[Seq[A], Yielder[Many.UnitStream[A, V, W]]] = manyInit(List(init))(init)(p)

  /** Create a Piglet that returns many values, each created according to the given Piglet. */
  def manyInit[A, V, W](inits: Seq[A])(init: A)(p: (A => Piglet[A, V => W])): Piglet[Seq[A], Yielder[Many.UnitStream[A, V, W]]] = {
    val s = Stream(Success(inits))
    val _init = p(init)
    val m = new Many.UnitStream[A, V, W](p, s, _init, init)
    Piglet(s, SimpleYielder(m))
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
  def confirm[A, B, C, D, E](init: A)(validate: Piglet[A, (Stream[A] => B) => B] => Piglet[A, (C => D => (C, D)) => Stream[A] => E])(nomatch: String): Piglet[A, Yielder[E]] = {
    val first = `yield`(init).mapView(_.apply[B] _)
    val second = `yield`(init).mapView(_.apply[E] _)
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

  def petPiglet(init: Pet): Piglet[Pet, (Stream[Species] => (Stream[String] => Stream[String])) => Stream[String]] = {
    val pn = init.name.toP.mapView(_.toFn[Stream[String]]).validate("Please enter the pet's name")(_ != "")
    val ps = init.species.toP.mapView(_.toFn[Stream[String] => Stream[String]])
    ps :@: pn apRet { s: Species => n: String => Pet(s, n) }
  }

  def personPiglet[X](init: Person): Piglet[Person, (Stream[String] => (Stream[String] => (Many.UnitStream[Pet, Stream[Species] => (Stream[String] => Stream[String]), Stream[String]] => Submitter[Person] => X))) => X] = {
    type SS = Submitter[Person] => X
    type M = Many.UnitStream[Pet, Stream[Species] => (Stream[String] => Stream[String]), Stream[String]]
    val pf = init.first.toP.mapView(_.toFn[Stream[String] => M => SS]).validate("Please enter a first name")(_ != "")
    val pl = init.last.toP.mapView(_.toFn[M => SS]).validate("Please enter a last name")(_ != "")
    val pp: Piglet[Seq[Pet], (M => SS) => SS] = Piglet.many[Pet, Stream[Species] => (Stream[String] => Stream[String]), Stream[String]](defaultPet)(petPiglet).mapView(_.toFn[SS])
    val pp2 = (pf :@: pl :@: pp) apRet { first => last => pets => Person(first, last, pets) }
    val pv = pp2.validate("Unknown user")(p => dictionary.contains((p.first, p.last)))
    Piglet.withSubmit(pv)
  }

  val initUser = Person("Alonzo", "Church", Nil)

}
