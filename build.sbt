crossScalaVersions in ThisBuild := Seq("2.11.7", "2.10.5")

scalaVersion in ThisBuild := "2.11.7"

organization in ThisBuild := "cc.co.scala-reactive"

scalacOptions in (ThisBuild, Compile, compile) += "-deprecation"
