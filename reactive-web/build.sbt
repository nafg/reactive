name := "reactive-web-base"

description := "FRP-based abstractions to control the browser from the server"

libraryDependencies += "net.liftweb" %% "lift-util" % "2.6.2" exclude("ch.qos.logback","logback-classic")

libraryDependencies += "com.lihaoyi" %% "upickle" % "0.2.8"

libraryDependencies += "org.scalatest"  %% "scalatest"  % "2.2.5"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
