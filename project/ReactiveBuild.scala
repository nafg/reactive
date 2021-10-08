import scala.sys.process._

import sbt.Keys._
import sbt._


object ReactiveBuild {
  val sonatypeStaging = "https://oss.sonatype.org/service/local/staging/deploy/maven2/"

  def branchName = "git rev-parse --abbrev-ref HEAD".!!.trim

  val defaults = Seq(
    resolvers += Resolver.sonatypeRepo("snapshots"),
    checksums in update := Nil,
    Compile / doc / scalacOptions ++= Seq(
      "-sourcepath",
      baseDirectory.value.getAbsolutePath,
      "-doc-source-url",
      s"http://github.com/nafg/reactive/blob/$branchName/${baseDirectory.value.getName}€{FILE_PATH}.scala",
      "-doc-title",
      "Scaladocs - scala-reactive", "-groups"
    ),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),
    autoAPIMappings := true
  )

  val publishingSettings = defaults ++ Seq(
    publishTo := Some("Reactive Bintray" at "https://api.bintray.com/maven/naftoligug/maven/reactive/;publish=1"),
    publishMavenStyle := true,
    credentials ++= {
      sys.env.get("BINTRAYKEY").toSeq map (key =>
        Credentials(
          "Bintray API Realm",
          "api.bintray.com",
          "naftoligug",
          key
        )
        )
    },
    pomExtra := <developers><developer><id>nafg</id></developer></developers>,
    homepage := Some(url("https://github.com/nafg/reactive")),
    licenses := Seq(("Modified Apache", url("https://github.com/nafg/reactive/blob/master/LICENSE.txt"))),
    scmInfo := Some(ScmInfo(
      url("https://github.com/nafg/reactive"),
      "scm:git:git://github.com/nafg/reactive.git",
      Some("scm:git:git@github.com:nafg/reactive.git")
    ))
  )

  val nonPublishingSettings = defaults :+ (publish / skip := true)
}

object Dependencies {
  val liftVersion = "3.5.0"
}
