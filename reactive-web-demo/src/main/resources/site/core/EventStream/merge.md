### Merging `EventStream`s: `|`

You can also get the union of two `EventStreams`. The
new `EventStream` will fire all events that either of the
original `EventStream`s fire.

<pre class="brush:scala">
val allClicks = leftClicks | middleClicks | rightClicks
</pre>

 <div data-lift="DemoPane?snippet=EventStream_union"></div>
