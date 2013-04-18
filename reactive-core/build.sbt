name := "reactive-core"

organization := "cc.co.scala-reactive"

version := "0.2-SNAPSHOT"

description := "An FRP framework"

unmanagedSourceDirectories in Compile <++= (scalaBinaryVersion, baseDirectory) { (sv, bd) => Seq(bd / "src" / "main" / ("scala-"+sv)) }
