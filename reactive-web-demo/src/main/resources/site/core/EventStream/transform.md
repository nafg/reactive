### Transformations

What makes `EventStream`s very versatile is their set
of transformation methods, which return `EventStream`s whose
events are based on the original `EventStream` with
some modification. You can chain together various transformations to get
a `EventStream` whose event values are very different than those
of the original `EventStream`. This is similar to how
you can transform collections: `List(1,2,3).map(_ * 10).filter(_ < 25)`. Consumers of the
resulting `EventStream` don’t need to care about how it
relates to the original `EventStream`. Whenever the original `EventStream`
fires an event, the transformed `EventStream`s may fire their own events
that are based on the original’s event, according to the transformation.
The way this works behind the scenes is that a listener function is added to
the "parent" `EventStream` that simply fires the correct
event(s) from the "child" `EventStream`.

#### Finer-grained lifetime control: `takeWhile`

What if you need finer-grained control over your function’s
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
however there’s nothing stopping you from doing so if you so desire.

 <div data-lift="DemoPane?snippet=EventStream_takeWhile"></div>

#### A more focused `EventStream`: `filter`

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

 <div data-lift="DemoPane?snippet=EventStream_filter"></div>

#### A completely transformed `EventStream`: `map`

Another fundamental collections method is `map`. It
lets you transform a collection by applying any function to each
element; in the returned collection, each element will have the value
calculated by applying the function to the corresponding element in the
original collection.

So too, you can `map` `EventStream`s. You
pass `map` a function, and for every event that the parent `EventStream`
fires, the new `EventStream` will fire an event whose value
is the result of applying the function to the parent’s event.

 <div data-lift="DemoPane?snippet=EventStream_map"></div>

#### Combined `filter` and `map`: `collect`

For convenience, `EventStream` has a `collect`
method, just like Scala’s collections. Just as you’d expect, it takes a
`PartialFunction` that specifies which events to respond
to and what event to fire in response.

#### Switching `EventStream`s: `flatMap`

Let’s get a bit more advanced now, and apply `flatMap`
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

Of course, it’s possible to return new `EventStream`s
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

 <div data-lift="DemoPane?snippet=EventStream_flatMap"></div>

#### <a id="foldLeft"></a>Passing on state: `foldLeft`

What if you need the way you handle events to depend on various
factors, in a way that’s more complex than what `flatMap`
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

 <div data-lift="DemoPane?snippet=EventStream_foldLeft"></div>
