### Adding listeners: `foreach`

Under the hood `EventStream` manages a collection of
listeners, but that’s not how you should think of it conceptually.
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

 <div data-lift="DemoPane?snippet=EventStream_foreach"></div>
