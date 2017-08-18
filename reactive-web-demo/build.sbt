name := "reactive-web-demo"

description := "reactive-web demo app"

enablePlugins(JettyPlugin)

libraryDependencies += "net.liftweb" %% "lift-markdown" % Dependencies.liftVersion

unmanagedResourceDirectories in Compile += (scalaSource in Compile).value
