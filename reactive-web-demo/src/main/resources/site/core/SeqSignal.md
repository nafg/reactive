### `SeqSignal`

Here is one of the most important and powerful features of `reactive`.
Suppose you have a list of contacts displayed. Whenever a contact is
added, removed, or updated, you need to update the display. Well, you
keep the list in a `Signal[Seq[Contact]]`, listen to change
events, and update the display in response. You could have the display
be represented by a `map`ped `Signal`. But it’s
expensive, especially with a lot of contacts, to update the entire
display whenever a single row changes.

For this reason, there is a special subtype of `Signal`,
`SeqSignal`. It adds, on top of `Signal`’s
members, an additional `EventStream[SeqDelta]` called `deltas`.
Depending on how you create the first `SeqSignal` in the
chain, it will fire `SeqDelta` events representing inserts,
removes, and updates. In addition multiple operations may be batched in
a single event, so for example multiple deletions do not need to result
in multiple, separate repaints. When you `map` or `flatMap`
a `SeqSignal`, the resulting `SeqSignal` fires
delta events whose relationship to the delta events fired by the orignal
`SeqSignal` is defined by the function you pass to `map`
or `flatMap`.

Remember that `SeqSignal[T]` extends `Signal[Seq[T]]`.
So if you want to transform the elements a `SeqSignal`, you
need to nest one `map` inside another: you have to pass the `map`
method on `SeqSignal[T]` / `Signal[Seq[T]]` a
function that operates on a `Seq[T]`, and since you want to
operate on the elements inside that `Seq`, you need to call `map`
on _it_.

<pre class="brush:scala">
// When in.deltas fires an Insert of 3,
// the returned SeqSignal's deltas will
// fire an Insert of 30,
// besides that now and change will
// have the behavior inherited from Signal
def timesTen(in: SeqSignal[Int]) =
  in.map { _ map (_ * 10)}
</pre>

### Creating a `SeqSignal`

How do you get a `SeqSignal`?

One way is subtype `BufferSignal`. The easiest way to
create a `BufferSignal` is with its factory (see example
below). You can modify it in two ways: Either use `def value`,
which returns an `ArrayBuffer` which you can mutate directly,
with each mutation resulting in the corresponding deltas being fired; or
use the same syntax you use to update `Var`s (see example),
which will result in diff being calculated between the old and the new
values. You can override `def comparator` to change the
equality test used by the diffing algorithm.

<pre class="brush:scala">
val bufferSignal = BufferSignal(1, 2, 3, 4, 5)
bufferSignal.value += 6  // fires an Insert
bufferSignal ()= List(2, 3, 4, 5, 6, 7)  // fires a Remove and an Insert
</pre>

The other way is to use the `SeqSignal` factory, which
takes a `Signal[Seq[T]]` and returns a `SeqSignal[T]`
whose deltas are calculated by running the diffing algorithm on every
change. To create an immutable `SeqSignal`, just pass it a `Val`.

<pre class="brush:scala">
val simpleSignal = Var(List(1,2,3))
val seqSignal = SeqSignal(simpleSignal)
simpleSignal ()= List(2,3,4) // seqSignal.deltas fires
                             // a Remove and an Insert.
</pre>
