name := "reactive-web-demo"

description := "Reactive-web demo app (http://scalareactive.org)"

enablePlugins(JettyPlugin)

libraryDependencies += "net.liftweb" %% "lift-markdown" % Dependencies.liftVersion

unmanagedResourceDirectories in Compile += (scalaSource in Compile).value
