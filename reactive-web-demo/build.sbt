name := "reactive-web-demo"

description := "Reactive-web demo app (http://scalareactive.org)"

enablePlugins(JettyPlugin)

libraryDependencies += "net.liftweb" %% "lift-markdown" % "2.6.2"

webappPostProcess := { outDir =>
  val docs = outDir / "api"
  IO.copyDirectory((doc in(core, Compile)).value, docs / "reactive-core")
  IO.copyDirectory((doc in(routing, Compile)).value, docs / "reactive-routing")
  IO.copyDirectory((doc in(transport, Compile)).value, docs / "reactive-transport")
  IO.copyDirectory((doc in(jsdsl, Compile)).value, docs / "reactive-jsdsl")
  IO.copyDirectory((doc in(web_base, Compile)).value, docs / "reactive-web-base")
  IO.copyDirectory((doc in(web_html, Compile)).value, docs / "reactive-web-html")
  IO.copyDirectory((doc in(web_widgets, Compile)).value, docs / "reactive-web-widgets")
  IO.copyDirectory((doc in(web, Compile)).value, docs / "reactive-web")
  IO.copyDirectory((doc in(web_lift, Compile)).value, docs / "reactive-web-lift")
  IO.copyDirectory((doc in(root, ScalaUnidoc)).value, docs / "unidoc")
}

unmanagedResourceDirectories in Compile += (scalaSource in Compile).value
