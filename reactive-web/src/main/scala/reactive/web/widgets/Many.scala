package reactive
package web
package widgets

import reactive.Subscription

import Result.{ Failure, Success }

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
