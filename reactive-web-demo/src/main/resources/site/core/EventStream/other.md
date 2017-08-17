### Preventing infinite loops: `nonrecursive`

If an `EventStream` firing can result in that `EventStream`
firing again, you may end up with infinite recursion (in other words, a `StackOverflowError`).
If that is the case, call `nonrecursive`, which returns
a derived `EventStream` that uses a `DynamicVariable` (Scala’s `ThreadLocal`)
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
at a time, and the thread firing the event won’t block. See below for an example.

### Detecting that an event has been superseded: `zipWithStaleness`

Sometimes, like when you have a long-running, hard-working event handler, you may want
to check whether a new event has been fired since the handler began running. Perhaps
if one indeed was, you want to short-circuit and skip the rest of the work (maybe because
it’s superfluous in light of the new event). This may be especially important if you are using
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
