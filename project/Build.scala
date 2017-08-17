import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport._

object ReactiveBuild extends Build {
  val sonatypeStaging = "https://oss.sonatype.org/service/local/staging/deploy/maven2/"

  def branchName = "git rev-parse --abbrev-ref HEAD".!!.trim

  val defaults = Seq(
    resolvers += Resolver.sonatypeRepo("snapshots") ,
    checksums in update := Nil,
    scalacOptions in (Compile, doc) ++= Seq(
      "-sourcepath",
      baseDirectory.value.getAbsolutePath,
      "-doc-source-url",
      s"http://github.com/nafg/reactive/blob/$branchName/${ baseDirectory.value.getName }€{FILE_PATH}.scala",
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

  lazy val routing = (crossProject.crossType(CrossType.Full) in file("reactive-routing"))
    .settings(
      name := "reactive-routing",
      description := "Type safe routing library",
      scalacOptions in(Compile, doc) ++= Seq("-implicits", "-implicits-show-all"),
      libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

    )
    .settings(publishingSettings: _*)
  lazy val routingJS = routing.js
  lazy val routingJVM = routing.jvm

  lazy val transport = (project in file("reactive-transport"))
    .settings(publishingSettings: _*)
    .dependsOn(core)

  lazy val jsdsl = (project in file("reactive-jsdsl"))
    .settings(publishingSettings: _*)
    .dependsOn(transport)

  lazy val web_base = (project in file("reactive-web"))
    .settings(publishingSettings: _*)
    .dependsOn(core, jsdsl)

  lazy val web_html = (project in file("reactive-web-html"))
    .settings(publishingSettings: _*)
    .dependsOn(web_base)

  lazy val web_widgets = (project in file("reactive-web-widgets"))
    .settings(publishingSettings: _*)
    .dependsOn(web_html)

  lazy val web = (project in file("reactive-web-aggregated"))
    .settings(publishingSettings: _*)
    .settings(name := "reactive-web")
    .dependsOn(web_widgets)

  lazy val web_lift = (project in file("reactive-web-lift"))
    .settings(publishingSettings: _*)
    .dependsOn(web_widgets, routingJVM)

  lazy val web_demo = (project in file("reactive-web-demo"))
    .settings(nonPublishingSettings: _*)
    .dependsOn(web_lift)

  lazy val root = (project in file("."))
    .settings(nonPublishingSettings: _*)
    .aggregate(
      core,
      transport,
      jsdsl,
      web_base,
      routingJS,
      routingJVM,
      web_html,
      web_widgets,
      web,
      web_lift,
      web_demo
    )
}
