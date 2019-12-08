name := "reactive-web-base"

description := "FRP-based abstractions to control the browser from the server"

libraryDependencies += "net.liftweb" %% "lift-util" % Dependencies.liftVersion

libraryDependencies += "io.circe" %% "circe-parser" % "0.11.2"

libraryDependencies += "org.scalatest"  %% "scalatest"  % "3.1.0"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.2" % "test"
