name := "reactive-web"

organization := "cc.co.scala-reactive"

version := "0.2-SNAPSHOT"

description := "FRP-based abstractions for Ajax and Comet"

resolvers ++= Seq(
        "snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
        "releases"      at "http://oss.sonatype.org/content/repositories/releases"
)

{
  val liftVersion = "2.5-SNAPSHOT"
  libraryDependencies ++= Seq(
    "javax.servlet" % "servlet-api" % "2.5" % "test",
    "net.liftweb" %% "lift-testkit" % liftVersion exclude("ch.qos.logback","logback-classic"),
    "net.liftweb" %% "lift-webkit" % liftVersion exclude("ch.qos.logback","logback-classic")
  )
}
