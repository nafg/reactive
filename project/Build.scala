import sbt._
import Keys._
import /*com.github.siasia.*/WebPlugin._

object ReactiveBuild extends Build {
  val pomCommon = <xml:group>
    <url>http://reactive-web.tk</url>
    <licenses></licenses>
    <scm>
      <connection>scm:git:git://github.com/nafg/reactive.git</connection>
      <developerConnection>scm:git:git@github.com:nafg/reactive.git</developerConnection>
      <url>git@github.com:nafg/reactive.git</url>
    </scm>
    <developers></developers>
  </xml:group>
  def pom(name: String, desc: String) =
    <name>{name}</name> ++ <description>{desc}</description> ++ pomCommon

  name := "reactive-parent"
  organization := "cc.co.scala-reactive"
  version := "0.0.1-SNAPSHOT"

  val sonatype = "http://oss.sonatype.org/content/repositories/"
  val sonatypeSnapshots = sonatype+"snapshots/"

  resolvers += ScalaToolsSnapshots
  resolvers += "Sonatype snapshots" at sonatypeSnapshots
  scalaVersion := "2.8.1"

  publishTo <<= (version) { version: String =>
    if (version.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at sonatypeSnapshots)
    else
      Some("releases" at sonatype+"releases/")
  }
  publishMavenStyle := true
  credentials += Credentials(Path("/private")/"nafg"/".credentials")

  val liftVersion = "2.4-SNAPSHOT"

  lazy val reactive_core = Project("reactive-core", file("reactive-core")) settings(
    version := "0.0.1-SNAPSHOT",
    pomExtra := pom("reactive-core", "An FRP framework"),
    libraryDependencies <+= scalaVersion { scalaVersion =>
      val scalatestVersions = Map("2.8.1"->"1.5", "2.9.0"->"1.6.1")
      "org.scalatest" %% "scalatest" % scalatestVersions(scalaVersion)
    }
  )
  lazy val reactive_web = Project("reactive-web", file("reactive-web")) settings(
    pomExtra := pom("reactive-web", "FRP-based abstractions for Ajax and Comet"),
    version := "0.0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "net.liftweb" %% "lift-testkit" % liftVersion,
      "net.liftweb" %% "lift-webkit" % liftVersion
    ),
    resolvers += ScalaToolsSnapshots,
    resolvers += "Sonatype snapshots" at sonatypeSnapshots
  ) dependsOn(reactive_core)
  lazy val reactive_web_demo = Project("reactive-web-demo", file("reactive-web-demo")) settings(webSettings: _*) settings(
    version := "0.0.1-SNAPSHOT",
    jettyScanDirs := Nil,
    libraryDependencies += ("org.mortbay.jetty" % "jetty" % "6.1.26" % "jetty,test")
  ) dependsOn(reactive_web)
}
