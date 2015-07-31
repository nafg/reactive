name := "reactive-web-demo"

description := "Reactive-web demo app (http://scalareactive.org)"

enablePlugins(JettyPlugin)

libraryDependencies += "net.liftweb" %% "lift-markdown" % "2.6.2"

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
resourceGenerators in Compile <+= (resourceManaged in Compile, doc in (core, Compile)) map {
  (dir, docs) =>
  val dest = dir / "copied-docs" / "reactive-core-api"
  IO.copyDirectory(docs, dest)
  Seq[File]()
}

// Automatically copy the Web API Docs into the "copied-docs" managed resource folder
resourceGenerators in Compile <+= (resourceManaged in Compile, doc in (web, Compile)) map {
  (dir, docs) =>
  val dest = dir / "copied-docs" / "reactive-web-api"
  IO.copyDirectory(docs, dest)
  Seq[File]()
}

// Automatically copy the web-lift API Docs into the "copied-docs" managed resource folder
resourceGenerators in Compile <+= (resourceManaged in Compile, doc in (web_lift, Compile)) map {
  (dir, docs) =>
  val dest = dir / "copied-docs" / "reactive-web-lift-api"
  IO.copyDirectory(docs, dest)
  Seq[File]()
}

// Automatically copy the routing API Docs into the "copied-docs" managed resource folder
resourceGenerators in Compile <+= (resourceManaged in Compile, doc in (routing, Compile)) map {
  (dir, docs) =>
  val dest = dir / "copied-docs" / "reactive-routing-api"
  IO.copyDirectory(docs, dest)
  Seq[File]()
}

// Automatically copy the unified API Docs into the "copied-docs" managed resource folder
resourceGenerators in Compile <+= (resourceManaged in Compile, doc in (root, ScalaUnidoc)) map {
  (dir, docs) =>
  val dest = dir / "copied-docs" / "unidoc"
  IO.copyDirectory(docs, dest)
  Seq[File]()
}

// Use the copied-docs folder as an addition to the webapp resources.
// When the resourceGenerators run, this dir will contain
// "reactive-core-api" and "reactive-web-api" directories.
unmanagedResourceDirectories in Compile += (resourceManaged in Compile).value / "copied-docs"
