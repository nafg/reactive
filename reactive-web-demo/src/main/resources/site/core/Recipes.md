

## Recipes

### Double-click

Sometimes you want to know if an event happened twice in a row
within a short amount of time. A common example is detecting a
double click in a GUI.

<pre class="brush: scala">
  clicks.map(_ =&gt; System.currentTimeMillis)
    .foldLeft((0L,0L)) {
      case ((_, lastT), newT) =&gt; (lastT, newT)
    }.collect{ case (lastT, newT) if newT-lastT &lt; 500 =&gt; () }
</pre>

### EventStreams of different event types

Sometimes one needs to deal with events of the type
`EventStream[(X,Y)]`
where
`X`
represents the event type and
`Y`
is the event value, and one needs to keep previous state for each
event type for further calculations. This is not as straightforward
as the previous case. When the possibilities for X are known, the
simplest solution may be to derive an event stream for each
possibility containing only the events of that type, using
`filter`. Another possible solution is to keep the last two values and times
per type of event using
`Map`s:

<pre class="brush: scala">
def lastValues[X, Y](x: EventStream[(X, Y)], defaultX: X) = x.foldLeft((Map.empty[X, (Y, Long)], Map.empty[X, (Y, Long)], defaultX)) {
  case ((mapBeforeLast, mapLastT, b), (a, v)) =&gt; (mapLastT, mapLastT updated(a, (v, System.currentTimeMillis)), a)
}.collect {
  case ((mapLastT, mapNewT, a)) =&gt;
    for (
      (x1, t1) &lt;- mapNewT.get(a);
      (x0, t0) &lt;- mapLastT.get(a)
    ) yield (ctl, (x1, t1), (x0, t0))
}

val x = new EventSource[(String,Double)]

//double clicks:
val doubleClicks = lastValues(x, "").collect {
  case Some( (key, (x1, t1), (x0, t0)) ) if t1 - t0 &lt; 500 =&gt; "double click from: "+key
}

doubleClicks.foreach(println)

x.fire( ("mouse",2.0) )
x.fire( ("mouse",4.0) )
x.fire( ("trackpad",2.3) )
x.fire( ("trackpad",5.3) )
x.fire( ("joystick",5.3) )

//results
//double click from: mouse
//double click from: trackpad
</pre>

