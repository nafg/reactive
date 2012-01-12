libraryDependencies <++= (scalaVersion) { v => List(
    "org.scalatest" %% "scalatest" % (if(v startsWith "2.8") "1.5" else "1.6.1") % "test" withSources,
    "org.scala-tools.testing" %% "scalacheck" % (if(v startsWith "2.8") "1.8" else "1.9") % "test" withSources
  )
}

unmanagedSourceDirectories in Compile <++= (scalaVersion, baseDirectory) { (sv, bd) => Seq(bd / "src" / "main" / ("scala-"+sv)) }
