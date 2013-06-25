## Signal

### Introduction

While `EventStream` represents a stream of discrete
values over time --- that is, each value only exists instantaneously (in
practice that means that you can never say "what is the current
value?"), `Signal` represents a continuous value. In
practical terms, a `Signal` has a current value, and an `EventStream`
that, whenever the `Signal`'s value changes, fires the new
value.

### `now`

You can get the current value of a `Signal` by calling
`now`. However, functional style means that most of the time
it should be avoided, and you should find a way to have `Signal`
call your code with the value rather than you asking the `Signal`
for it.

### `change`

This is an `EventStream` that fires an event for every
change to the `Signal`'s value.

<pre class="brush:scala">
signal.change.foreach {v =&gt;
  assert(v == signal.now)
}
</pre>

### Creating a `Signal`

The two simplest `Signal` classes share the names of
two important Scala keywords. They are `Val` and `Var`.
`Val` lets you create an immutable `Signal`.
Calling `now` will always return the value it was created
with. `change` will never fire an event. `Var`
lets you create a mutable `Signal`. You can update it by writing
`myVar () = newValue`, which is syntactic sugar for
`myVar.update(newValue)`.

<pre class="brush:scala">
object MyApp extends Observing {
  val myVal = Val(72)
  val myVar = Var(31)
  myVar.change foreach println
  myVar ()= 29  // prints 29
}
</pre>

You can create a `Signal` from an existing `EventStream`
by calling its `hold` method. And you can create a
`Signal` by transforming another `Signal` via
`map` and `flatMap`.

### `foreach`

Just as you can react to events with `EventStream#foreach`, you can
do something for all values of a `Signal` with `foreach`.
Passing a function to `Signal#foreach` is equivalent to executing
the function and then calling `foreach` on the `change`
`EventStream` with the function.

### `map`

Just like you can `map` an `EventStream` to
get a new, transformed, `EventStream`, you can `map`
`Signal`s too. The resulting `Signal` will reflect
the transformation expressed in the mapping function, both in its
definition of `now` and in the events `change`
fires.

<pre class="brush:scala">
val myVar = Var(3)
val mapped = myVar.map(_ * 10)
println(mapped.now)  // prints 30
myVar ()= 62
println(mapped.now)  // prints 620
</pre>

 <a class="btn btn-primary" target="_blank" href="/showdemo/Signal_map">Live Example</a>

### <a id="flatMap">`flatMap`</a>

Just like you can `flatMap` an `EventStream`
to get an `EventStream` that "switches" between several `EventStream`s,
so too you can create a `Signal` whose value depends on
several other `Signal`s. However, there are several
differences from `EventStream`'s `flatMap`, and
its usage is slightly different. These differences stem from the fact
that a `Signal` always has a value. So the semantics are,
that initially the resulting `Signal` has the value of the `Signal`
created by applying the function passed to `flatMap` to the
current value of the parent `Signal`. This is reflected
in `now` as well as in `change`.

<pre class="brush:scala">
val myVar1 = Var(72)
val myVar2 = Var(69)
val myVar3 = Var(false)

val flatMapped = myVar3 flatMap {
  case true =&gt; myVar1
  case false =&gt; myVar2
}
println(flatMapped.now)  // prints 72
myVar3 ()= true
println(flatMapped.now)  // prints 69
myVar2 ()= 2
myVar1 ()= 1
println(flatMapped.now)  // prints 2
myVar3 ()= false
println(flatMapped.now)  // prints 1

</pre>

Another example: filtering a list based on an entered string.

<pre class="brush: scala">
def filteredList(filterSignal: Signal[String], itemsSignal: Signal[Seq[String]]) =
  for {
    filter &lt;- filterSignal
    items &lt;- itemsSignal
  } yield items.filter(s =&gt; s.indexOf(filter) &gt;= 0)
/* The above desugars to:
filterSignal.flatMap{ filter =&gt;
  itemsSignal.map{ items =&gt;
    items.filter(s =&gt; s.indexOf(filter) &gt;= 0)
  }
}
*/
</pre>

Similarly, you can pass `flatMap` a function that returns
`EventStream`s, and `flatMap` will return an
`EventStream` that "strings together"
the event streams that correspond to the signal's values.

For instance, suppose you have an application that swaps the
meaning of the mouse buttons while the `Alt` key is
depressed. You have two `EventStream`s, one firing left mouse button clicks,
and one firing right mouse button clicks, and a `Signal` representing
the `Alt` key's state.

<pre class="brush:scala">
val selectClicks = altKey flatMap (if(_) leftButtonClicks else rightButtonClicks)
val contextClicks = altKey flatMap (if(_) rightButtonClicks else leftButtonClicks)
</pre>

 <a class="btn btn-primary" target="_blank" href="/showdemo/Signal_flatMap">Live Example</a>

### Passing down state: `foldLeft`

If you need to derive a signal based on memory of previous state,
use `foldLeft`. It works similar to [EventStream.foldLeft](EventStream#foldLeft).

### `zip`

A method that can be convenient sometimes is `zip`. A parallel to the collections' `zip` method,
it allows you to create a `Tuple2`-valued `Signal` from two `Signal`s.

<pre class="brush:scala">
def nameAndAge(name: Signal[String], age: Signal[Int]): Signal[(String,Int)] = name zip age
</pre>

### Preventing infinite loops: `distinct`, `nonrecursive`

When you have two interdependent signals, you need a way to prevent infinite loops
(signal A caused signal B to change which causes signal A to change). `Signal`
has two methods that return a new `Signal` identical to the parent `Signal`
but with an added safety filter. `distinct` returns a new `Signal` that filters out change events that are equal
to the signal's previous value. This suffices in most cases. But what if when signal A causes signal B
to change signal A, it sets to another value, infinitely? A silly illustration:

<pre class="brush:scala">myVar.map(_ + 1) >> myVar</pre>

Another example is where rounding errors are not symmetric. For such scenarios, call `nonrecursive`, which
uses a `DynamicVariable` (Scala's `ThreadLocal`) to prevent recursion.

### Time-consuming handling of values: `nonblocking`, `zipWithStaleness`

Similar to `EventStream`, `Signal` has `nonblocking` and `zipWithStaleness` methods.

### Merging a variable number of signals: `sequence`

How can you keep the total cost of several orders up to date? If you store it as a `Signal[Seq[Signal[Order]]]`, you can use `sequence`:

<pre class="brush:scala">
orders.sequence.map(_.sum)
</pre>
