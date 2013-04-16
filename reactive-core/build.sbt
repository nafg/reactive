name := "reactive-core"

description := "An FRP framework"

unmanagedSourceDirectories in Compile <++= (scalaBinaryVersion, baseDirectory) { (sv, bd) =>
	resolveDirs(bd / "src" / "main" / ("scala-"+sv))
}
