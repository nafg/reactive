import sbt._
import Keys._
import com.github.siasia.WebPlugin._
import com.github.siasia.PluginKeys._

object ReactiveBuild extends Build {
  val pomCommon = <xml:group>
    <url>http://reactive-web.tk</url>
    <licenses><license><name>Modified Apache</name></license></licenses>
    <scm>
      <connection>scm:git:git://github.com/nafg/reactive.git</connection>
      <developerConnection>scm:git:git@github.com:nafg/reactive.git</developerConnection>
      <url>git@github.com:nafg/reactive.git</url>
    </scm>
    <developers><developer><id>nafg</id></developer></developers>
  </xml:group>
  def pom(name: String, desc: String) =
    <name>{name}</name> ++ <description>{desc}</description> ++ pomCommon

  val sonatypeSnapshots = "http://oss.sonatype.org/content/repositories/snapshots/"
  val sonatypeStaging = "https://oss.sonatype.org/service/local/staging/deploy/maven2/"

  val defaults = Defaults.defaultSettings ++ Seq(
    organization := "cc.co.scala-reactive",
    version := "0.2-SNAPSHOT",
    resolvers ++= List(
      ScalaToolsSnapshots,
      "Sonatype snapshots" at sonatypeSnapshots,
      "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"
    ),
    scalacOptions += "-deprecation",
    scalaVersion := "2.9.0-1",
    checksums in update := Nil
  )
  val publishingDefaults = defaults ++ Seq(
    publishTo <<= (version) { version: String =>
      if (version.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at sonatypeSnapshots)
      else
        Some("staging" at sonatypeStaging)
    },
    publishMavenStyle := true,
    credentials += Credentials(file("/private/nafg/.credentials"))
  )


  val liftVersion = "2.4-SNAPSHOT"

  lazy val reactive_core = Project(
    "reactive-core",
    file("reactive-core"),
    settings = publishingDefaults ++ Seq(
      pomExtra := pom("reactive-core", "An FRP framework"),
      libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"
    )
  )
  lazy val reactive_web = Project(
    "reactive-web",
    file("reactive-web"),
    settings = publishingDefaults ++ Seq(
      pomExtra := pom("reactive-web", "FRP-based abstractions for Ajax and Comet"),
      libraryDependencies ++= Seq(
        "javax.servlet" % "servlet-api" % "2.5" % "test",
        "net.liftweb" %% "lift-testkit" % liftVersion,
        "net.liftweb" %% "lift-webkit" % liftVersion
      )
    )
  ) dependsOn(reactive_core) aggregate(reactive_core)
  lazy val reactive_web_demo = Project(
    "reactive-web-demo",
    file("reactive-web-demo"),
    settings = defaults ++ webSettings ++ Seq(
      libraryDependencies += ("org.mortbay.jetty" % "jetty" % "6.1.26" % "container,test"),
      scanDirectories in Compile := Nil,
      publishArtifact := false
    )
  ) dependsOn(reactive_web) aggregate(reactive_web)
  lazy val root = Project(
    "root",
    file("."),
    settings = defaults ++ Seq(
      publishArtifact := false
    )
  ) dependsOn(reactive_web_demo) aggregate(reactive_web_demo)
}
