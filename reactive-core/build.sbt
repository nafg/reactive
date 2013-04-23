name := "reactive-core"

description := "An FRP framework"

unmanagedSourceDirectories in Compile <++= (scalaBinaryVersion, baseDirectory) { (sv, bd) =>
	val mappedVersions = sv match {
		case "2.9.1" | "2.9.2" | "2.10" | "2.10.0-RC2" => List("2.9.1")
		case _ => Nil
	}
	mappedVersions map { mv => bd / "src" / "main" / ("scala-"+mv)}
}
