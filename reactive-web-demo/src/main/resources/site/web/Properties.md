## Properties/Attributes

Events are the mechanism by which events get from
the browser to your code running on the server.
There is another mechanism that builds on events,
and that is properties or attributes. This
allows you to access and modify the browser's
state. The property's state is sent to
your code on the server with events.
Modifications to properties are sent to the
browser with
the JavaScript queuing mechanism.

### `DomProperty`

`DomProperty` acts a proxy to a property or attribute of a DOM element.
It has an `EventStream[String]` that fires updates to the property's value.
And it has an `update` method that sets the property's value with a `JsExp`.

Like `DomEventSource`, you can apply it to an Elem and get a new Elem, or use it
as a `NodeSeq=>NodeSeq` function for CSS Selectors. Unlike `DomEventSource` however, you need to either
call `render`, or rely on the implicit conversion to do so. In the case of `DomProperty`, `render`
returns an element like the one passed in, with an added attribute
corresponding the property and its value.
In addition, any events it was linked to by
calling `withEvents` will also
be applied to the element. Also, if the element
does not have an `id` attribute
one will be added. `render` (and the implicit conversion) require an
implicit `Page`. The element's id will be
stored in the `DomProperty` for that
`Page`, so that update commands can be sent to the browser.

### PropertyVar

Usually you will not use `DomProperty` directly. It fires raw values as `String`s,
accepts new values as `JsExp`s, and does not maintain a current value. Instead,
most of the time you will use the type-safe `PropertyVar`. `PropertyVar` extends `Var`
and wraps a `DomProperty`. Setting its value will will result in the corresponding
  property being changed in the browser on whichever `Page`s
  are registered with this `DomProperty` (whether via Ajax or
  Comet) except for the `Page` that is currently firing an
  Ajax event that is setting its value, if any.

The simplest way to instantiate a `PropertyVar`
is by writing `PropertyVar("name")(initialValue)`. If the
attribute has a different name than the javascript property, you can
specify it as a second parameter after the name.

<pre class="brush:scala">
  class Snippet {
    val value = PropertyVar("value")("")
    def render =
      "input" #> value
  }
</pre>

That factory takes an implicit `PropertyCodec` parameter, which specifies how to
represent the type in the browser. There are default implicit instances
for `String`, `Int`, `Option[Int]` (`None` is represented as `-1`),
and `Boolean` (`false` generates no attribute, `true` generates an attribute with the value set to its name,
as in `checked="checked"`).

Here are some members of `DomProperty` and `PropertyVar`:

#### `updateOn`
Calling this method with one or more `DomEventSource`s will
  register a `JsEventStream` with it that fires the property's value whenever
  the browser raises that event. When `Page`s
  are subsequently added to the `DomProperty` (for instance when it's rendered),
  updates to the property will reach the server. In the case of `DomProperty`
  they will be fired from its `values` event stream, and in the case of `PropertyVar`
  it will contain the new value. Returns the `DomProperty` or `PropertyVar`.
  
#### `withEvents`
Like `updateOn`, plus
  the `DomProperty` will automatically
  add the events to any element it is rendered to. Returns the `DomProperty` or `PropertyVar`.
  
#### `render`
Returns a `NodeSeq=>NodeSeq` binding function. Requires an implicit Page.

#### `render(Elem)`
Applies the property to the Elem and returns a new Elem. Requires an implicit Page.

