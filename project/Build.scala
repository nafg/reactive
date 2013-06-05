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

  val sonatypeSnapshots = "http://oss.sonatype.org/content/repositories/snapshots/"
  val sonatypeStaging = "https://oss.sonatype.org/service/local/staging/deploy/maven2/"

  val defaults = Defaults.defaultSettings ++ Seq(
    organization := "cc.co.scala-reactive",
    version := "0.3.2-SNAPSHOT",
    resolvers ++= List(
      "Sonatype snapshots" at sonatypeSnapshots,
      "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"
    ),
    checksums in update := Nil,
    scalacOptions in (Compile, compile) += "-deprecation",
    (scalacOptions in (Compile, doc) <++= (baseDirectory).map{ bd =>
      Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "http://github.com/nafg/reactive/tree€{FILE_PATH}.scala")
    }),
    scalaVersion := "2.10.0",
    libraryDependencies ++= List(
      "org.scalatest" %% "scalatest" % "2.0.M6-SNAP5" % "test",
      "org.scalacheck" %% "scalacheck" % "1.10.1-SNAPSHOT" % "test" cross CrossVersion.full,
      "org.scala-lang" % "scala-actors" % "2.10.0"
    ),
    testOptions in Test += Tests.Argument("-oF")
  )
  val publishingDefaults = defaults ++ Seq(
    publishTo <<= (version) { version: String =>
      if (version.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at sonatypeSnapshots)
      else
        Some("staging" at sonatypeStaging)
    },
    publishMavenStyle := true,
    credentials ++= {
      val f = file("/private/nafg/.credentials")
      if(f.exists) Seq(Credentials(f))
      else Nil
    }
  )

  lazy val reactive_core = Project(
    "core",
    file("reactive-core"),
    settings = publishingDefaults ++ Seq(
      pomExtra := pomCommon
    )
  )
  lazy val reactive_web = Project(
    "web",
    file("reactive-web"),
    settings = publishingDefaults ++ Seq(
      pomExtra := pomCommon
    )
  ) dependsOn(reactive_core)
  lazy val reactive_web_demo = Project(
    "demo",
    file("reactive-web-demo"),
    settings = defaults ++ Seq(
      publishArtifact := false
    )
  ) dependsOn(reactive_web)
  lazy val root = Project(
    "root",
    file("."),
    settings = defaults ++ Seq(
      publishArtifact := false
    )
  ) aggregate(reactive_core, reactive_web, reactive_web_demo)
}
