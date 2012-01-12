seq(webSettings: _*)

libraryDependencies += ("org.mortbay.jetty" % "jetty" % "6.1.26" % "container,test")

scanDirectories in Compile := Nil

libraryDependencies <++= (scalaVersion) { v => List(
    "org.scalatest" %% "scalatest" % (if(v startsWith "2.8") "1.5" else "1.6.1") % "test" withSources,
    "org.scala-tools.testing" %% "scalacheck" % (if(v startsWith "2.8") "1.8" else "1.9") % "test" withSources
  )
}
