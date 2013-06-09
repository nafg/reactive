name := "reactive-web"

description := "FRP-based abstractions for Ajax and Comet"

libraryDependencies += "javax.servlet" % "servlet-api" % "2.5" % "test"

{
  val liftVersion = "2.5"
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-testkit" % liftVersion exclude("ch.qos.logback","logback-classic"),
    "net.liftweb" %% "lift-webkit" % liftVersion exclude("ch.qos.logback","logback-classic")
  )
}
