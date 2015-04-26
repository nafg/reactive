name := "reactive-routing"

description := "Type safe routing configuration"

scalacOptions in (Compile, doc) ++= Seq("-implicits", "-implicits-show-all")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test"
