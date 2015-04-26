name := "reactive-transport"

description := "Abstraction over server/browser means of transport"

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
    libraryDependencies.value ++ Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
    )
    case Some((2, 10)) =>
      libraryDependencies.value
  }
}

libraryDependencies += "net.liftweb" %% "lift-json" % "2.6" exclude("ch.qos.logback","logback-classic")
