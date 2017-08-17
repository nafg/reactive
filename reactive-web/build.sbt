name := "reactive-web-base"

description := "FRP-based abstractions to control the browser from the server"

libraryDependencies += "net.liftweb" %% "lift-util" % "2.6.3" exclude("ch.qos.logback","logback-classic")

libraryDependencies += "com.lihaoyi" %% "upickle" % "0.4.4"

libraryDependencies += "org.scalatest"  %% "scalatest"  % "3.0.3"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
