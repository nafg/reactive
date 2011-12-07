libraryDependencies += "org.scalatest" %% "scalatest" % "1.5"

unmanagedSourceDirectories in Compile <++= (scalaVersion, baseDirectory) { (sv, bd) => Seq(bd / "src" / "main" / ("scala-"+sv)) }
