name := "reactive-web-base"

description := "FRP-based abstractions to control the browser from the server"

libraryDependencies += "net.liftweb" %% "lift-util" % Dependencies.liftVersion

libraryDependencies += "io.circe" %% "circe-parser" % "0.11.2"

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.1.3.0" % Test
