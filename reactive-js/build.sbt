name := "reactive-js"

description := "Write javascript in scala"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
