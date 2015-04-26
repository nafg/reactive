name := "reactive-transport"

description := "Abstraction over server/browser means of transport"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.2")
    case Some((2, 10)) =>
      Seq()
  }
}

libraryDependencies += "net.liftweb" %% "lift-json" % "2.6.2" exclude("ch.qos.logback","logback-classic")
