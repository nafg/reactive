name := "reactive-jsdsl"

description := "Type safe DSL for constructing javascript expressions and statements"

libraryDependencies += "net.liftweb" %% "lift-util" % "2.6.3" exclude("ch.qos.logback","logback-classic")

libraryDependencies += "org.scalatest"  %% "scalatest"  % "2.2.5"  % "test"
