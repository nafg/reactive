import ReactiveBuild._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}


crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.17")

scalaVersion in ThisBuild := "2.12.17"

organization in ThisBuild := "cc.co.scala-reactive"

scalacOptions in ThisBuild ++=
  Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Ywarn-unused-import",
    "-Ywarn-unused",
    "-Ywarn-value-discard"
  )

enablePlugins(ScalaUnidocPlugin)

unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(routingJS, web_demo)

lazy val core = (project in file("reactive-core"))
  .settings(publishingSettings: _*)

lazy val routing = (crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full) in file("reactive-routing"))
  .settings(
    name := "reactive-routing",
    description := "Type safe routing library",
    scalacOptions in(Compile, doc) ++= Seq("-implicits", "-implicits-show-all"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.4" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.2" % Test
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
  .settings(
    webappPostProcess := {
      val projsDocs =
        Def.task {
          (projectID.value.name, (Compile / doc).value)
        }
          .all(
            ScopeFilter(inProjects(core, routingJVM, transport, jsdsl, web_base, web_html, web_widgets, web, web_lift))
          )
          .value

      val unidocs = (root / ScalaUnidoc / doc).value

      { outDir =>
        val docs = outDir / "api"

        println("Copying docs to " + docs)

        for ((proj, doc) <- projsDocs)
          IO.copyDirectory(doc, docs / proj)

        IO.copyDirectory(unidocs, docs / "unidoc")
      }
    }
  )

lazy val root: Project = (project in file("."))
  .settings(name := "reactive-root", nonPublishingSettings)
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
    web_lift
  )
