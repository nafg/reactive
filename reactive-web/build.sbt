name := "reactive-web-base"

description := "FRP-based abstractions to control the browser from the server"

libraryDependencies += "net.liftweb" %% "lift-util" % Dependencies.liftVersion

libraryDependencies += "io.circe" %% "circe-parser" % "0.8.0"

libraryDependencies += "org.scalatest"  %% "scalatest"  % "3.0.3"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
