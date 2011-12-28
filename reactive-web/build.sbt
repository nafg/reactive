{
  val liftVersion = "2.4-SNAPSHOT"
  libraryDependencies ++= Seq(
    "javax.servlet" % "servlet-api" % "2.5" % "test",
    "net.liftweb" %% "lift-testkit" % liftVersion withSources,
    "net.liftweb" %% "lift-webkit" % liftVersion withSources,
    "cc.co.scala-reactive" %% "reactive-core" % "0.2-SNAPSHOT" withSources
  )
}
