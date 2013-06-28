seq(webSettings: _*)

libraryDependencies += ("org.mortbay.jetty" % "jetty" % "6.1.26" % "container,test")

libraryDependencies += "net.liftweb" %% "lift-markdown" % "2.6-SNAPSHOT"

scanDirectories in Compile := Nil

// To change the default port, use the following line
// (port in container.Configuration) := 9999

// Automatically copy the scala source directory into a managed resources folder,
// to use as runtime resources under the "/scala-sources" path
resourceGenerators in Compile <+= (resourceManaged in Compile, scalaSource in Compile) map { (dir, src) =>
  val copiedSources = dir / "scala-sources"
  IO.copyDirectory(src, copiedSources)
  Path.allSubpaths(copiedSources).map { case (f, p) => f }.toSeq
}

// Automatically copy the Core API Docs into the "copied-docs" managed resource folder
resourceGenerators in Compile <+= (resourceManaged in Compile, doc in (reactive_core, Compile)) map {
  (dir, docs) =>
  val dest = dir / "copied-docs" / "reactive-core-api"
  IO.copyDirectory(docs, dest)
  Seq[File]()
}

// Automatically copy the Web API Docs into the "copied-docs" managed resource folder
resourceGenerators in Compile <+= (resourceManaged in Compile, doc in (reactive_web, Compile)) map {
  (dir, docs) =>
  val dest = dir / "copied-docs" / "reactive-web-api"
  IO.copyDirectory(docs, dest)
  Seq[File]()
}

// Use the copied-docs folder as an addition to the webapp resources.
// When the resourceGenerators run, this dir will contain
// "reactive-core-api" and "reactive-web-api" directories.
webappResources in Compile <+= (resourceManaged in Compile) { dir =>
  dir / "copied-docs"
}
