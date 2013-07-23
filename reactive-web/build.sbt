name := "reactive-web"

description := "FRP-based abstractions for Ajax and Comet"

libraryDependencies += "net.liftweb" %% "lift-util" % "2.5" exclude("ch.qos.logback","logback-classic")

libraryDependencies += "net.liftweb" %% "lift-webkit" % "2.5" exclude("ch.qos.logback","logback-classic")
