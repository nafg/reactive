{
  val liftVersion = "2.4-SNAPSHOT"
  libraryDependencies ++= Seq(
    "javax.servlet" % "servlet-api" % "2.5" % "test",
    "net.liftweb" %% "lift-testkit" % liftVersion withSources,
    "net.liftweb" %% "lift-webkit" % liftVersion withSources,
    "cc.co.scala-reactive" %% "reactive-core" % "0.2-SNAPSHOT" withSources
  )
}

libraryDependencies <++= (scalaVersion) { v => List(
    "org.scalatest" %% "scalatest" % (if(v startsWith "2.8") "1.5" else "1.6.1") % "test" withSources,
    "org.scala-tools.testing" %% "scalacheck" % (if(v startsWith "2.8") "1.8" else "1.9") % "test" withSources
  )
}
