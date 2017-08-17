crossScalaVersions in ThisBuild := Seq("2.11.8", "2.10.6")

scalaVersion in ThisBuild := "2.11.8"

organization in ThisBuild := "cc.co.scala-reactive"

scalacOptions in (ThisBuild, Compile, compile) += "-deprecation"

import sbtunidoc.Plugin._
import UnidocKeys._
unidocSettings

unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(routingJS, web_demo)
