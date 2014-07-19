import sbt._
import Keys._

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

  val sonatypeSnapshots = "https://oss.sonatype.org/content/repositories/snapshots/"
  val sonatypeStaging = "https://oss.sonatype.org/service/local/staging/deploy/maven2/"

  val defaults = Defaults.defaultSettings ++ Seq(
    organization := "cc.co.scala-reactive",
    resolvers ++= List(
      "Sonatype snapshots" at sonatypeSnapshots,
      "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"
    ),
    checksums in update := Nil,
    scalacOptions in (Compile, compile) += "-deprecation",
    (scalacOptions in (Compile, doc) <++= (baseDirectory).map{ bd =>
      val sourceUrl = "http://github.com/nafg/reactive/blob/master/" + bd.getName + "€{FILE_PATH}.scala"
      Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", sourceUrl, "-doc-title", "Scaladocs - scala-reactive", "-groups")
    }),
    libraryDependencies ++= List(
      "org.scalatest" %% "scalatest" % "2.1.2" % "test",
      "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
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

  lazy val reactive_routing = Project(
    "routing",
    file("reactive-routing"),
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
  ).dependsOn(reactive_core)

  lazy val reactive_web_lift = Project(
    "web-lift",
    file("reactive-web-lift"),
    settings = publishingDefaults ++ Seq(
      pomExtra := pomCommon
    )
  ).dependsOn(reactive_web, reactive_routing)

  lazy val reactive_web_demo = Project(
    "demo",
    file("reactive-web-demo"),
    settings = defaults ++ Seq(
      publishArtifact := false
    )
  ) dependsOn(reactive_web_lift)

  lazy val root = Project(
    "scala-reactive",
    file("."),
    settings = defaults ++ Seq(
      publishArtifact := false
    )
  )
  .settings(sbtunidoc.Plugin.unidocSettings: _*)
  .aggregate(reactive_core, reactive_web, reactive_web_lift, reactive_web_demo, reactive_routing)
}
