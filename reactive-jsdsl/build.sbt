name := "reactive-jsdsl"

description := "Type safe DSL for constructing javascript expressions and statements"

libraryDependencies += "net.liftweb" %% "lift-util" % "2.6.2" exclude("ch.qos.logback","logback-classic")

libraryDependencies += "org.scalatest"  %% "scalatest"  % "2.2.5"  % "test"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7" % "test"
