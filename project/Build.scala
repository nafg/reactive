import sbt._
import Keys._

object ReactiveBuild extends Build {
  val sonatypeSnapshots = "https://oss.sonatype.org/content/repositories/snapshots/"
  val sonatypeStaging = "https://oss.sonatype.org/service/local/staging/deploy/maven2/"

  val defaults = Seq(
    resolvers ++= List(
      "Sonatype snapshots" at sonatypeSnapshots,
      "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"
    ),
    checksums in update := Nil,
    scalacOptions in (Compile, doc) ++= Seq(
      "-sourcepath",
      baseDirectory.value.getAbsolutePath,
      "-doc-source-url",
      s"http://github.com/nafg/reactive/blob/master/${ baseDirectory.value.getName }€{FILE_PATH}.scala",
      "-doc-title",
      "Scaladocs - scala-reactive", "-groups"
    ),
    testOptions in Test += Tests.Argument("-oF")
  )

  val publishingSettings = defaults ++ Seq(
    publishTo := (
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at sonatypeSnapshots)
      else
        Some("staging" at sonatypeStaging)
    ),
    publishMavenStyle := true,
    credentials ++= {
      val f = file("/private/nafg/.credentials")
      if(f.exists) Seq(Credentials(f))
      else Nil
    },
    pomExtra := <developers><developer><id>nafg</id></developer></developers>,
    homepage := Some(url("http://scalareactive.org")),
    licenses := Seq(("Modified Apache", url("https://github.com/nafg/reactive/blob/master/LICENSE.txt"))),
    scmInfo := Some(ScmInfo(
      url("https://github.com/nafg/reactive"),
      "scm:git:git://github.com/nafg/reactive.git",
      Some("scm:git:git@github.com:nafg/reactive.git")
    ))
  )

  val nonPublishingSettings = defaults :+ (publish := ())

  lazy val core = (project in file("reactive-core"))
    .settings(publishingSettings: _*)

  lazy val routing = (project in file("reactive-routing"))
    .settings(publishingSettings: _*)

  lazy val transport = (project in file("reactive-transport"))
    .settings(publishingSettings: _*)
    .dependsOn(core)

  lazy val web = (project in file("reactive-web"))
    .settings(publishingSettings: _*)
    .dependsOn(core, transport)

  lazy val web_lift = (project in file("reactive-web-lift"))
    .settings(publishingSettings: _*)
    .dependsOn(web, routing)

  lazy val web_demo = (project in file("reactive-web-demo"))
    .settings(nonPublishingSettings: _*)
    .dependsOn(web_lift)

  lazy val root = (project in file("."))
    .settings(nonPublishingSettings: _*)
    .settings(sbtunidoc.Plugin.unidocSettings: _*)
    .aggregate(
      core,
      transport,
      web,
      routing,
      web_lift,
      web_demo
    )
}
