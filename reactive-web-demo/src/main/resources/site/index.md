<div class="hero-unit">
  <h1>Reactive Web</h1>
  <h2>Simplify Your Lift</h2>
</div>

## About

Reactive-web is a new framework for writing highly interactive and
dynamic web applications. It’s written in [Scala](http://scala-lang.org), sits on top of [Lift](http://liftweb.net), and uses the Functional
Reactive Programming library reactive-core (it’s in the same
repository).

As in GWT, you can code the user interface in the same language
as the rest of your application (except
in Scala instead of Java), rather than writing JavaScript. Unlike
GWT, however, you don’t need an extra build step to convert your code
to JavaScript. You can easily combine code that runs on the
browser with code that runs on the server.
And, you can declare dynamic relationships between components, like
binding in Flex/JavaFX/etc. (only much more powerful).

Your webapp can interact with browser elements as if they were
regular objects inside the JVM. You can handle events from DOM
elements in your code, and you can update DOM objects by mutating
the corresponding object. And you can assign dynamically evaluated
expressions, and the browser will update automatically (more
easily and powerfully than with Lift’s built in wiring).
These relationships can be enforced from within the browser,
or they can involve the server if need be.

Reactive has powerful support for incremental updates. So you
can have a set of items transformed it in all sorts of ways and
rendered, and when the set of items changes, only the necessary
changes will be made to the DOM.

It is extremely testable. You can write Selenium-style tests —
but they don’t require starting up a browser or Jetty, and your
tests can access and interact with both the "user interface"
as well as the server-side objects (such as the contents
of a snippet class).

You can use it within a regular Lift application, in one place
or throughout. Or you can use it to completely change the kind of
webapps you write. No longer do webapps have to be less dynamic than
GUI applications. (You can even use it to build a rich desktop GUI
app — just run Jetty embedded!)

Its functional design works very well with the Lift template
binding mechanisms, so you
can keep view design completely separate from your code.

Updates that are computed on the server can arrive at the browser
as part of the original page load, as part of an ajax response
(if they occurred during an ajax call), via Lift’s
comet mechanism, or via Html5 EventSource. It all happens automatically, so you
don’t have to worry about which; reactive-web will select
the one that’s most appropriate.

To find out more about reactive-core and reactive-web, just
explore the menu bar above. The Getting Started guide is
[here](/web/GettingStarted).

You can discuss reactive on the [scala-user](mailto:scala-user@googlegroups.com) mailing list.
Looking forward to your feedback!

The code is at [http://github.com/nafg/reactive](http://github.com/nafg/reactive).
Note that while there is still much to be done, it’s more in terms
of completeness (like adding more elements and properties, and more
features) than in terms of plumbing or usability.

Some planned features include:

* Onsubmit event, so you can use it when JavaScript is not enabled or supported
* JavaScript signals (event streams are already implemented)
* Widgets, from scratch and/or ExtJS etc.
