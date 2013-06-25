## Events

Reactive-web is to a large extent about browser events.
By events we mean any event the browser can
fire for a given element, such as mouse
interactions and keyboard input. Often
you want to react to events directly. In addition,
element properties are propagated to the server via events.

### `DomEvent`

`DomEvent`
is a simple trait that represents the event object that is raised by the browser. It is extended
by case classes and case objects that represent the various
DOM events and their state. When the browser fires an event, `reactive-web`
can fire a corresponding DomEvent instance to your code.

### `DomEventSource`

Let's move on to `DomEventSource`.
`DomEventSource` wraps an `JsEventStream` that fires
values that encode the event data. This lets you
handle the event on the client side, without requiring an
ajax call to the server. In addition, `DomEventSource`
has an `EventStream` instance (a `lazy val`,
implemented by calling `toServer` on the `JsEventStream`)
that fires `DomEvent` instances. It provides the
ability to generate an event handler attribute that fires the
`JsEventStream`.

You can create a `DomEventSource` simply by
instantiating it with the event type as a type parameter:

<pre class="brush: scala">
new DomEventSource[Focus]
</pre>

Additionally, the `DomEventSource`
companion object contains convenience methods,
so you can just write, e.g., `DomEventSource.click`.

If you have an `Elem` you can
pass it to `EventStream`'s `apply`
method and get back a new `Elem`
with the event handler attribute. Also,
`DomEventSource` extends `NodeSeq=>NodeSeq`, so
you can use it with Lift's CSS Selectors or binding. For example:

<pre class="brush:xml">
&lt;!-- in the template --&gt;
&lt;button&gt;Delete!&lt;/button&gt;
</pre>
<pre class="brush:scala">
// in the snippet class
def render = {
  val click = DomEventSource.click
  for(_ &lt;- click.eventStream)
    confirm("Are you sure you want to delete it?"){
      case true => //delete it
    }
  "button" #> click
}
</pre>

Here are a few of its members:

#### `jsEventStream`
The `JsEventStream` that proxies the javascript event stream
  in the browser.


#### `eventStream`
The lazily instantiated `EventStream` that propagates `jsEventStreams`'s 
  values to the server as `DomEvent`s.

#### `addEventData`
Supplies an additional `JsEventStream` to be fired in response to this event,
  and a `JsExp` to calculate the value to be fired.


In addition, `DomEventSource` extends `Forwardable` and
`JsForwardable`, so you can use their convenience operators to link
together various reactive objects.
