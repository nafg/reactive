libraryDependencies <+= (scalaVersion) { v =>
   "org.scalatest" %% "scalatest" % (if(v startsWith "2.8") "1.5" else "1.6.1")
}

unmanagedSourceDirectories in Compile <++= (scalaVersion, baseDirectory) { (sv, bd) => Seq(bd / "src" / "main" / ("scala-"+sv)) }
