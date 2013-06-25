## Getting Started with reactive-web

### 1. Choose an existing Lift project or [create a new one](http://www.assembla.com/wiki/show/liftweb/Using_SBT)

### 2. Add reactive-web as a dependency
For instance, if you're using sbt, that would be:
````
  libraryDependencies += "cc.co.scala-reactive" %% "reactive-web" % "0.4-SNAPSHOT"
````

### 3. Enable it
  In `Boot.scala` in `def boot` add one or more of the following lines:
  <pre class="brush: scala">
  import reactive.web._
  AppendToRender.init()  // required
  LiftCometPage.init()   // to enable server push via a comet actor
  SimpleAjaxPage.init()  // to enable non-lift ajax
  SsePage.init()         // to enable server push via html5 server-side events</pre>

### 4. Declare `Page`s in your snippet classes
  In each snippet (a class that is instantiated when a page is being rendered, with the same instance not being reused for another page load),
  you must instantiate an implicit `Page` via one of the following:
  <pre class="brush: scala">
  implicit val page = LiftAjaxPage()    // for ajax only, via lift ajax
  implicit val page = LiftCometPage()   // for ajax and push, via lift ajax and comet
  implicit val page = SimpleAjaxPage()  // for ajax only, via non-lift ajax
  implicit val page = SsePage()         // for ajax and push, via non-lift ajax and html5 SSE</pre>

### 5. Start coding!

*   You will probably want to import `reactive._`
and `reactive.web._`
*   Snippets that react to events and signals may need to extend `Observing`
*   Read the rest of the site for more info.
