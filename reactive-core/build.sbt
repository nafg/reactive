name := "reactive-core"

description := "An FRP framework"

unmanagedSourceDirectories in Compile <++= (scalaVersion, baseDirectory) { (sv, bd) => Seq(bd / "src" / "main" / ("scala-"+sv)) }
