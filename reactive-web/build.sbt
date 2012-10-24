name := "reactive-web"

description := "FRP-based abstractions for Ajax and Comet"

{
  val liftVersion = "2.5-SNAPSHOT"
  libraryDependencies ++= Seq(
    "javax.servlet" % "servlet-api" % "2.5" % "test",
    "net.liftweb" %% "lift-testkit" % liftVersion exclude("ch.qos.logback","logback-classic") cross CrossVersion.full,
    "net.liftweb" %% "lift-webkit" % liftVersion exclude("ch.qos.logback","logback-classic") cross CrossVersion.full
  )
}
