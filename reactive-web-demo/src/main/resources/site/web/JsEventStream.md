## Javascript

### `JsEventStream`

`reactive-web` contains a facility to set up FRP reactions that are contained completely
within the client side. You control this with the `JsEventStream` class. `JsEventStream`
proxies a corresponding javascript object, and you can call familiar event stream methods such as
`fire`, `foreach`, `map`, `flatMap`, and `filter`.
Calling these methods in scala triggers a call to the corresponding javascript method in the browser.
Of course, mostly you only call these methods to initialize the event streams — once they are wired
properly they work on their own — so from then on they do not need to communicate between the browser and
the server. The exception is `fire`, which does make sense to call later, allowing you to
control what happens in the browser by inserting a value into an event stream.

On the other hand, if the server needs to respond to a `JsEventStream`, call its `toServer`
method, which returns a regular `EventStream`. The values will be encoded in JSON format
when they are sent to the browser. You can pass `toServer` a `JValue=>U` function, where `U`
is the type of the new `EventStream` (`JValue` being `lift-json`’s AST representation).
Alternatively, you can implicitly provide `toServer` with a `net.liftweb.json.Formats` and a `Manifest[U]`,
and `lift-json` will do the extracting.

### Javascript DSL

Since `fire`, `foreach`, `map`, `flatMap`, and `filter` are proxy methods
that really execute in javascript, you need to call them with values that can readily be rendered as javascript. To facilitate
this, `reactive-web` contains a DSL for writing typesafe javascript expressions and statements in Scala. Here are some of the basics:

*   Javascript expressions have type `JsExp[T <: JsTypes.JsAny]`. Subclasses of `JsExp` include
    `JsLiteral`, `JsIdent`, `JsRaw` (there are others as well). All take a type parameter
    that indicates what the javascript type of the expression will be after it’s evaluated. `JsExp` defines
    a `render` method that returns the javascript it represents as a `String`.
*   Many scala values can be converted into javascript with the `$` postfix operator. For instance, `"A string".$`
    yields a `JsLiteral[JsString]`, and `'symbol.$[JsString]` yields a `JsIdent[JsString]` named `symbol`.
*   A scala function of type `JsExp[P] => JsExp[R]` (a.k.a. `JsExp[P] => $[R]`) can represent a javascript function literal
    consisting of a return statement and an expression.
    For instance, `(i: JsExp[JsInt]) => i + 1.$` can automatically be converted into `function(arg){return arg+1}`.
    (The way this works is simple: `reactive-web` invokes the function with a `JsIdent[JsInt]` named `arg`. `JsExp` has a +
    method that returns a new `JsExp` that represents the addition of itself and its argument. This `JsExp`
    is then returned from your function to `reactive-web`, which inserts that into the template of a javascript function literal.)
*   Similarly, a function `JsExp[P] => JsStatement` creates a function that contains any number of statements.
*   You can represent entire javascript APIs in scala by defining a trait that extends `JsStub`, and getting an instance of it by writing
    `jsProxy[myStubTrait]()`. `jsProxy` creates a dynamic proxy class that intercepts calls to abstract methods whose
    parameter types are all `JsExp` (or a subtype), and invokes the method of the same name in the browser.
    Note that the name of the trait is assumed to be the name of the identifier on which to call the method. If you need to distinguish,
    call `jsProxy[myStubTrait]('name)` or `jsProxy[myStubTrait]("name")`. See below for more on `JsStub`.

 <div data-lift="DemoPane?snippet=JsEventStreamDemo"></div>

The following snippet should give you a good idea how to write Javascript statements with the DSL. Wrapping them in a `Javascript` block sends it to the browser to run.

<pre class="brush:scala">
Javascript {
  // if / else if / else
  If(true) {
    window.alert("True")
  }.ElseIf (false){
    window.alert("False")
  } Else {
    If(true) {
    } Else {
    }
  }
  // while, do / while
  While(true) {
    window.alert("Again!")
  }
  Do {
    window.alert("Hello!")
  } While (false)
  // switch / case
  Switch(1)(
    0.$ :> {
      window.alert("No")
    },
    1.$ :> window.alert("Yes")
  )
  // named var
  object i extends JsVar[JsNumber]
  // standard for loop
  For(List(i := 1), i < 10, List(i := i + 1)) {}
  // for / in -- note that this is a regular scala for-comprehension, on a $[JsArray[_]]
  for (i <- List(1.$, 2.$, 3.$)$) {
    If(i > 1) {
      window.alert("Greater")
    }
  }
  // for each / in (to iterate over elements, rather than indexes)
  for (i <- Each(List(1.$, 2.$, 3.$))) {
    If(i > 1) {
      window.alert("Greater")
    }
  }
  // throw, try / catch / finally
  Try {
    Throw("message")
  } Catch { c =>
  } Finally {
  }
  object myFunc extends Function({ x: $[JsNumber] =>
    If(x > 10) {
      window alert "Greater"
    } Else {
      window alert "Small"
    }
  })
  myFunc(10)
}
</pre>

### `JsStub`

`reactive-web` has a powerful mechanism that lets you define Javascript API stubs as scala traits.
You can then "call" Javascript APIs from scala code, simply by invoking methods on a proxy object.

The way it works is simple. You write a trait that extends `JsStub`, named after the Javascript object it represents,
and define abstract methods in it that correspond to the methods of that object.
The methods’ arguments should `JsExp`s, and they may return a `JsExp`, or another `JsStub` interface.
You then obtain an instance by calling `jsProxy[theTrait]()`.

<pre class="brush:scala">
sealed trait obj extends JsStub {
  def method(s: $[JsString]): $[JsString]
  def self: obj
  val prop: Assignable[JsNumber]
}
val obj = $$[obj]
Javascript {
  obj.method(obj.method("This is a scala string"))
  val v = JsVar[JsObj] := obj.self
  obj.prop := 2
}
</pre>
