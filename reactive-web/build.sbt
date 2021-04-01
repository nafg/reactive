name := "reactive-web-base"

description := "FRP-based abstractions to control the browser from the server"

libraryDependencies += "net.liftweb" %% "lift-util" % Dependencies.liftVersion

libraryDependencies += "io.circe" %% "circe-parser" % "0.11.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % Test
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test
