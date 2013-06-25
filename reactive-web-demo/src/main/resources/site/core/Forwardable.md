

## Forwardable

Several types, such as
`Signal`,
`EventStream`, and
`DomEventSource`, extend
`Forwardable`
. This gives you a convenient syntax to respond such objects' values
in several common ways.

#### Run a block of code:

 <pre class="brush: scala">eventStream ->> println("An event was fired")</pre>

#### Call a function:

<pre class="brush: scala">
eventStream =&gt;&gt; {v => println(v + " was fired")
eventStream += {v => println(v + " was fired")
</pre>

#### Call a partial function, when applicable:

<pre class="brush:scala">
eventStream ?>> {
  case n if n % 2 == 1 =&gt; println("An odd event")
}
</pre>

#### Assign the value to a `Var`:

<pre class="brush: scala">
  eventStream >> myVar
</pre>

#### Fire the value from an `EventSource`:

<pre class="brush: scala">
  eventStream &gt;&gt; eventSource
</pre>

These operators return the original object, so you can chain
them etc.

There is also the right-associative
`<<:`
operator, which lets you write forwarding expressions in the reverse
order. It returns the _target_ object.

<pre class="brush:scala">
// Add a click handler to a button html element
"button" #&gt; (click -&gt;&gt; alert("Mouse was clicked!"))

intEventStream =&gt;&gt; {i => println("Caught an event: "+i)}

// Update aVar with events from myEventSource,
// and propagate aVar's updates to anotherVar
(aVar &lt;&lt;: myEventSource) &gt;&gt; anotherVar
</pre>

#### Synchronize `Var`s

For convenience, you can synchronize the values of two
`Var`s with the
`<-->`
operator. It returns the `Var` on the left.

