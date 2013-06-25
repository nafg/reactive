## EventStream
### Introduction

`EventStream` is the central trait in reactive-core,
in the sense that everything else is built on it. It provides a
functional abstraction over the well-known listener pattern. Rather than
using imperative techniques, however, using `EventStream` you
can send or receive events using a more effective abstraction.

It is important to note that reactive does not handle threading
implicitly. Events are delivered on whichever thread they are fired, and
execution of that thread continues when all listeners have processed the
event.

One can think of an `EventStream` as being similar to a collection of
values, except that rather than all values existing simultaneously, each one
exists at a different point in time. Keep this analogy in mind, because
methods in reactive-core are named like the corresponding methods in the scala collections framework.

### Creating an `EventStream`

Often you will be dealing with `EventStream`s
obtained from a library. However, often you to need to create one
yourself. To do this just instantiate `EventSource`
(it extends `EventStream`).
You can then send events by calling `fire`.

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_EventSource">Live Example</a>

Of course, you also create a new `EventStream` every
time you call one of the transforming methods (below). And there's `Timer`.

### `Timer`

One useful subclass of `EventStream` is `Timer`.
It fires ticks at a given interval. Ticks'
values are based on the time when they are fired, not when they were
scheduled to fire.

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_Timer">Live Example</a>

### Adding listeners: `foreach`

Under the hood `EventStream` manages a collection of
listeners, but that's not how you should think of it conceptually.
Conceptually, by adding a listener what you really want is to execute a
function for each event fired. In other words, for all values of the `EventStream`.
So, just like when you want to execute a function for all values of a
Scala Collection you call `foreach`, you do the same here.
Unlike in the collections, though, `foreach` returns
immediately, and your function is saved to be executed whenever an event
fires. (Of course, you can use for-comprehension syntax instead of
calling `foreach` explicitly.)

This invites the question: How will the function, and any references
it has, ever be garbage collected (before the `EventStream`
is)? The answer is that `EventStream` keeps your function
inside a `WeakReference`, thus allowing it to be
garbage collected. If so, what stops it from being garbage
collected too soon? For that reason you must have an instance of `Observing`
in the implicits scope when calling `foreach` (and `forward`).
`EventStream` will place a strong reference to the function
inside the `Observing` object. Then the responsibility of
memory lifetime falls on the `Observing`. The easiest (and
often most correct) way to implement this, is to have the enclosing
class extend `Observing` (since it contains an implicit
reference to itself).

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_foreach">Live Example</a>

### Transformations

What makes `EventStream`s very versatile is their set
of transformation methods, which return `EventStream`s whose
events are based on the original `EventStream` with
some modification. You can chain together various transformations to get
a `EventStream` whose event values are very different than those
of the original `EventStream`. This is similar to how
you can transform collections: `List(1,2,3).map(_ * 10).filter(_ < 25)`. Consumers of the
resulting `EventStream` don't need to care about how it
relates to the original `EventStream`. Whenever the original `EventStream`
fires an event, the transformed `EventStream`s may fire their own events
that are based on the original's event, according to the transformation.
The way this works behind the scenes is that a listener function is added to
the "parent" `EventStream` that simply fires the correct
event(s) from the "child" `EventStream`.

### Finer-grained lifetime control: `takeWhile`

What if you need finer-grained control over your function's
lifetime than that afforded by `Observing`?

Well, recalling the collections analogy, what do you if you want
to act upon only part of a collection? Well, there are many ways, but
what if you want to process the beginning, stopping when a condition
evaluates to true? Of course, the answer is `takeWhile`. Same
here. And just like `takeWhile` on a collection returns a new
subset collection, so to `takeWhile` on an `EventStream`
returns a new `EventStream` that will only return events as
long as the condition evaluates to true.

You do not need an `Observing` in scope to use `takeWhile`.
As soon as an event fires in the original `EventStream` that
causes your condition to evaluate to false, it will cease to receive
events, and the reference to your predicate function and the new `EventStream`
will be removed.

Idiomatic code should not cause side effects in the predicate;
however there's nothing stopping you from doing so if you so desire.

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_takeWhile">Live Example</a>

### What's the point?

At this point you may be wondering why go through all these
hoops? Why not use the regular listener pattern directly, like Swing
does?

The short answer is that, while it's a new way of thinking, in
the long run it makes life easier. For example, if you take the
imperative approach of adding a listener in response to one situation,
in one method, and removing it in response to other situations, in other
methods, the result is that you have code that manages listeners'
lifecycles sprinkled in different places in code. Of course that means
it's harder to keep track of. By abstracting over the concept of
managing listeners' lifecycles, it becomes possible to program less in
terms of what the computer should do next, and more in terms of what you
want to happen, resulting in more maintainable code.

However, perhaps the greater advantage, is that we have a
general-purpose abstraction called an `EventStream` that can
be transformed and composed. This will become more apparent as you read
more about `EventStream` and about everything that interacts
with it and is built on it.

### A more focused `EventStream`: `filter`

What if you have an `EventStream` that fires a lot of
events but you are only interested in some? You can use `filter`.

As you have seen, given one `EventStream` it is
possible to derive a new `EventStream` that is "alive" only
until a certain point in time (based on a predicate). There are many
more operations that return transformed `EventStream`s. Most
mirror corresponding methods on collections.

For instance, just as one can `filter` a collection to
obtain a new collection that has only those elements that a predicate
matches, so too can you `filter` an `EventStream`
to obtain a new `EventStream` that fires only those events
which the predicate matches.

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_filter">Live Example</a>

### A completely transformed `EventStream`: `map`

Another fundamental collections method is `map`. It
lets you transform a collection by applying any function to each
element; in the returned collection, each element will have the value
calculated by applying the function to the corresponding element in the
original collection.

So too, you can `map` `EventStream`s. You
pass `map` a function, and for every event that the parent `EventStream`
fires, the new `EventStream` will fire an event whose value
is the result of applying the function to the parent's event.

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_map">Live Example</a>

### Combined `filter` and `map`: `collect`

For convenience, `EventStream` has a `collect`
method, just like Scala's collections. Just as you'd expect, it takes a
`PartialFunction` that specifies which events to respond
to and what event to fire in response.

### Switching `EventStream`s: `flatMap`

Let's get a bit more advanced now, and apply `flatMap`
to `EventStream`s.

What does `flatMap` do in the Scala Collections
Framework? In technical terms, it applies a **collection-valued**
function to all elements of a collection, and returns a new collection
that consists of all the collections returned by the function
concatenated. In practical terms, you can do interesting things like
stringing together several sequences, or flattening a sequence of
sequences. For instance:

<pre class="brush:scala">
val original = List(1, 2, 3)
val flatMapped = original.flatMap(x =&gt; List(x*10,x*10+1,x*10_2))
flatMapped == List(10,11,12,  20,21,22,  30,31,32)
</pre>

Similarly, by applying the above to the collection-over-time
analogy, `flatMap` allows you to create an `EventStream`
that fires events that are fired by different other `EventStream`s.
Note that no event will be fired until _after_ the first event
fired by the parent `EventStream` is received; then events
from the child `EventStream` will be fired until the next
parent event is received, after which new events will be fired as they
are received from the next child `EventStream`; and so on.
The simplest usage is to cycle through several other `EventStreams`.
For instance, suppose you want a shape to expand and then fade out.

<pre class="brush:scala">
// Assuming Shape is a case class with scale and opacity values
// and millisTimer fires events once per millisecond, starting
// at zero.
// Scale should animate from 0 to 1 over the first second,
// and opacity should animate from 1 to 0 over the next.
def compositeAnimation(millisTimer: EventStream[Long], shape: Shape): EventStream[Shape] = {
  val scale: EventStream[Double] =
    millisTimer.map(m =&gt; m/1000.0)
  val opacity: EventStream[Double] =
    millisTimer.map(m =&gt; 1 - (m-1000)/1000.0)
  val seconds = millisTimer.filter(_ % 1000 == 0).map(_ / 1000).
    takeWhile(_ < 2000)
  
  seconds.flatMap {
    case 0 =&gt; scale
    case 1 =&gt; opacity
  }
}
</pre>

Of course, it's possible to return new `EventStream`s
based on the event fired by the parent `EventStream`.

<pre class="brush:scala">
//assuming seconds fires an integer from 1 to 100 every
//second, incrementally
//we want to reverse each 10: (10,9..2,1, 20..11, 30..21 ...)
seconds.filter(_ % 10 == 1).flatMap{ t =&gt;
  seconds.map(s =&gt; t + 10 - s)
}
</pre>

You can also `flatMap` a `Signal` to
`EventStream`s. See [here](Signal#flatMap).

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_flatMap">Live Example</a>

### <a id="foldLeft"></a>Passing state: `foldLeft`

What if you need the way you handle events to depend on various
factors, in a way that's more complex than what `flatMap`
allows?

In imperative languages, a common task is to iterate through an
array etc., while keeping around a bunch of variables that keep track of
various things that can change depending on what you encounter as you
iterate over the array. Functional programming often accomplishes such
tasks using `foldLeft`. You pass `foldLeft` an
initial "state," and a function that takes the last "state" and the next
element in the collection. The function returns, for each element, what
the state should be for the next invocation. (Note that the functional
method allows for concurrent implementations, that do not require any
modifications to your code). For instance, take the common example of
totaling a list of numbers:

<pre class="brush:scala">
list.foldLeft(0){(totalSoFar, nextElement) =&gt; totalSoFar + nextElement}
//more commonly written as list.foldLeft(0)(_ + _)
</pre>

Similarly, you can call `foldLeft` on an `EventStream`,
and pass it an initial value, and a function that takes a "state" value
and an event and returns a new value. You will get back an `EventStream`
that fires the returned value (the same value that will be passed to the
next function invocation). we can create an `EventStream`
that, whenever a number is fired, fires the average value:

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_foldLeft">Live Example</a>

### Combining `EventStream`s: `|`

You can also get the union of two `EventStreams`. The
new `EventStream` will fire all events that either of the
original `EventStream`s fire.

<pre class="brush:scala">
val allClicks = leftClicks | middleClicks | rightClicks
</pre>

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_union">Live Example</a>

### Turning an `EventStream` into a `Signal`: `hold`

You can turn an `EventStream` into a `Signal`
via the `hold` method. You have to pass it an initial value
for the signal to hold until the next event fires.

 <a class="btn btn-primary" target="_blank" href="/showdemo/EventStream_hold">Live Example</a>

### Preventing infinite loops: `nonrecursive`

If an `EventStream` firing can result in that `EventStream`
firing again, you may end up with infinite recursion (in other words, a `StackOverflowError`).
If that is the case, call `nonrecursive`, which returns
a derived `EventStream` that uses a `DynamicVariable` (Scala's `ThreadLocal`)
to prevent it from firing recursively.


### `distinct`

`anEventStream.distinct` returns a derived event stream that only invokes it listeners for
events that are not equal to the previous event.

### Handling events on a different thread: `nonblocking`

Most `EventStream`s invoke all their listeners in whichever thread is firing the event,
thus blocking it. This can be problematic when handling an event is time-consuming.

Similarly, if a thread fires an event into an `EventStream` that
is is already handling an event from another thread, it will handle the new event on its thread
simultaneous with the other event handling already taking place. When handling
an event involves a lot of work, this means some of that work may be redundant.

Calling `nonblocking` on an `EventStream` returns a derived `EventStream`
that hands off events to an internal actor to process. Thus, only one event will be handled
at a time, and the thread firing the event won't block. See below for an example.

### Detecting that an event has been superseded: `zipWithStaleness`

Sometimes, like when you have a long-running, hard-working event handler, you may want
to check whether a new event has been fired since the handler began running. Perhaps
if one indeed was, you want to short-circuit and skip the rest of the work (maybe because
it's superfluous in light of the new event). This may be especially important if you are using
`nonblocking`: since only one event can be processed at a time, things may fall
more and more behind schedule. For instance, if processing a mouse click takes half a second,
after the user clicks 10 times it will take five seconds for the computer to "catch up" to the
user. One solution is to use `zipWithStaleness`. It will return an
<code>EventStream[(T, ()=&gt;Boolean)]</code>. In other words, each event will be a `Tuple2`
containing the actual event value together with a function. The function returns whether the
event has been "superseded."

<pre class="brush: scala">
for((click, isStale) <- mouseClicks.zipWithStaleness.nonblocking) {
  doSomeWork()
  if(!isStale()) doSomeMoreWork()
}
</pre>

### Splitting up events: `unzip` and `uneither`

When an `EventStream` fires data in pairs, it can be useful to be able to
get just the first half (or just the second half) of that pair. The `unzip`
method works on `EventStream` that fire pairs, and returns a pair of new 
`EventStreams`, where the first stream will fire events equal to the first
half of each pair fired by the original stream, and the second stream will fire events
equal to the second half of each pair fired by the original stream.

<pre class="brush: scala">
val pairedEvents: EventStream[(Int, String)] = //...
val (justTheInts, justTheStrings) = pairedEvents.unzip
for(i <- justTheInts) println("int " + i)
for(s <- justTheStrings) println("string " + s)
</pre>

`uneither` works similarly to `unzip`, but for `EventStreams`
that fire events as an `Either`.

<pre class="brush: scala">
val eitherEvents: EventStream[Either[Int, String]] = //...
val (justTheInts, justTheStrings) = eitherEvents.uneither
</pre>
