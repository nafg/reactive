name := "reactive-core"

description := "An FRP framework"

unmanagedSourceDirectories in Compile <++= (scalaBinaryVersion, baseDirectory) { (sv, bd) => Seq(bd / "src" / "main" / ("scala-"+sv)) }

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.2"

initialCommands in console := "import reactive._"

