## Testing

`Reactive-web`
has very powerful support for user interface testing. You can do
most things Selenium is usually used for, but you don’t have to run
a browser or Jetty, and your tests can have direct access to server
objects like snippet classes. Here’s a little taste:

<pre class="brush:scala">
implicit val testPage = new TestPage(self =>
  &lt;html&gt;&lt;body&gt;&lt;input id="phone"/&gt;&lt;/body&gt;&lt;/html&gt;
)
val ttt = testPage.testTransportType
def phone = tt.xml \\ "#phone"  // the element with id 'phone'
// type in it (it's a text input)
tt.sendKeys(phone, customer.homePhone)
// check that the snippet received the value we typed
snippet.phone.now should equal (customer.homePhone)
// check that the DOM was updated (the customer who's phone we typed was loaded)
tt.xml \\ ".firstname" attr "value" should equal (customer.firstName)
</pre>  

It builds on two things: (1) All updates in
`reactive`
are routed through pluggable `Transport`s; and
(2) DOM updates have a semantic (case class) representation.
The way it works is that `TestTransport` maintains
an XML tree (zipper actually) that
mocks a browser DOM’s state. Whenever a DOM update is called for,
it applies the transformation to its own XML. This way, you can
always tell what the browser’s DOM would look like. In addition, you
can simulate events, and it has a DSL that makes doing verifications on the DOM
very concise.

### Accessing the DOM and interacting with it

The current xml can be accessed as a
`NodeLoc` (an xml zipper structure)
by writing
`tt.xml`. You can get a scala.xml.Node via `tt.xml.node`, which lets you use all of Scala’s built in features to analyze it, such
as the simple XPath support available with
`\`
and
`\\`, and the collections’
`find`.
In addition `NodeLoc` provides some powerful querying tools itself. 
There are six searching operators, which are used
like this:
`node \ pred`, or like this:
`node \ (pred1, ... predN)`, and returns a node or nodes that satisfy all predicates.
Predicates are explained below. All six operators take a varargs, so
they can be used in either form.

The six operators are
`\`,
`\?`,
`\!`,
`\\`,
`\\?`, and
`\\!`.

`\\`
means search all descendants (and the node itself), while
`\`
means only search direct children.
`?`
means return an
`Option[NodeLoc]`,
`!`
that it returns a `NodeLoc` or if none was found throw an exception,
and no suffix means it returns an `Iterator[NodeLoc]`.

Any
`Node=>Boolean` can be used for a predicate. Also you write common predicates as
strings. You can write strings of the form
`"label"`,
`"#id"`,
`".class"`,
`":name"`,
`"=exact match of text"`, or
`"~substring of text"`. As an illustration, instead of the XPath expression
`//fieldset[legend/text()='Items']`
(as you might do with Selenium), you would write
`ts \\ ("fieldset", _ \? ("legend", "=Items") isDefined)`.
This means, find a node that passes two predicates. One, its label is "fieldset."
Two, the function passed as the second predicate
returns true for the node. That function searches the fieldset for a child node
whose label is "legend", and whose text content is "Items". Since it
uses
`\?`, the result, if any, is put in an
`Option`, which lets us call
`isDefined`    to get a
`Boolean`
indication of its presence, making it a valid predicate function.

As a shortcut, you can get a node by id just by writing
`testTransport("theId")`.

### Some members of `NodeLoc`:

#### `attr`
The attributes as a `Map[String, String]`

#### `update`
Replace the TestTransports DOM with one that has a different
attribute value in this node. The node itself is immutable and won’t have
the new value, but update looks up the new node and returns it.
This requires that the node has an id attribute (and assumes you aren’t
changing it). Usage: `node("attribute") = "value"`

#### `id`</dt>
<dd>The value of the id attribute

#### `clazz`
The value of the class attribute

#### `classes`
A `Set[String]` consisting of the
class attribute split on whitespace.

### Some members of `TestTransport`:

#### `update`
````
tt(tt("theId"), "href") = "#"
````
updates the specified attribute on the specified element.

#### `fire(nodeLoc, event)`
Simulate an event being fired on the node.
`event` is a `DomEvent`.
This works by analyzing the corresponding event attribute on the node
to determine the events and property updates that its fires.
Currently only implemented for Change, Click, and KeyUp.

#### `sendKeys(nodeLoc, text)`
Simulates typing in a field. `text`
may contain `'\b'` which is treated as a backspace.
For each character, it updates the `value attribute`
and fires a `KeyUp` event, and after it goes through the whole string it fires
a `Change` event.

#### `takeConfirm`
If a confirm box has been requested,
`TestTransport` detects it. `takeConfirm`
gets the "open" confirm box. It returns a tuple
containing the confirm message,
and a function of type `Boolean=>Unit`. Calling the
function will trigger the callback that would be
called when a button on the confirm box is clicked.


### Limitations as compared to Selenium

1.  No XPath. Instead of `"//fieldset[legend/text()='Items']"`, you do something like `ts.xml \ ("fieldset", _ \? "=Items" isDefined)`
2.  Mo support for alerts yet. Confirm boxes are supported.
3.  Can only test javascript generated by `reactive-web`.
4.  Since events are artificially simulated, there may be some differences to real life based on how browsers handle events.
5.  You can’t view the interaction. One solution is to render the xml at each step somewhere.
One option is to use something like Flying Saucer to render it to a Swing component. Another is to run the tests
from within a webapp and insert the xml into the DOM to display it.
