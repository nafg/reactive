name := "reactive-routes"

description := "Type safe routing configuration"

{
  val liftVersion = "2.5"
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion exclude("ch.qos.logback","logback-classic")
  )
}

scalacOptions in (Compile, doc) ++= Seq("-implicits", "-implicits-show-all") // for routing