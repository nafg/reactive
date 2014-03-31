## Getting Started with reactive-web and Lift

### 1. Choose an existing Lift project or [create a new one](http://www.assembla.com/wiki/show/liftweb/Using_SBT)

### 2. Add reactive-web-lift as a dependency
For instance, if you’re using sbt, that would be:
````
  libraryDependencies += "cc.co.scala-reactive" %% "reactive-web-lift" % "0.4.0-SNAPSHOT"
````

### 3. Enable it
  In `Boot.scala` in `def boot` add one or more of the following lines:
  <pre class="brush: scala">
  import reactive.web.lift._
  AppendToRender.init()  // required
  LiftCometPage.init()   // to enable server push via a comet actor
  SimpleAjaxPage.init()  // to enable non-lift ajax
  SsePage.init()         // to enable server push via html5 server-side events</pre>

### 4. Declare `Page`s in your snippet classes
  In each snippet (a class that is instantiated when a page is being rendered, with the same instance not being reused for another page load),
  you must instantiate an implicit `Page` with the requisite `TransportType`s. For instance:
  <pre class="brush: scala">
  // for ajax only, via lift ajax
  implicit val page = Page(
    new AppendToRenderTransportType(_),
    new LiftAjaxTransportType(_)
  )
  // for ajax and push, via lift ajax and comet
  implicit val page = Page(
    new AppendToRenderTransportType(_),
    new LiftAjaxTransportType(_),
    new LiftCometTransportType(_)
  )
  // for ajax only, via plain ajax
  implicit val page = Page(
    new AppendToRenderTransportType(_),
    new SimpleAjaxTransportType(_)
  )
  // for ajax and push, via plain ajax and html5 SSE
  implicit val page = Page(
    new AppendToRenderTransportType(_),
    new SimpleAjaxTransportType(_),
    new SseTransportType(_)
  )
  </pre>
  You can put the ones you will use inside an easily accessible `def` for convenience.

### 5. Start coding!

*   You will probably want to import `reactive._`, `reactive.web._`, and in some cases `reactive.web.lift._`
*   Snippets that react to events and signals may need to extend `Observing`
*   Read the rest of the site for more info.
