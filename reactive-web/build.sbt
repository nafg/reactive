name := "reactive-web"

description := "FRP-based abstractions for Ajax and Comet"

libraryDependencies += "net.liftweb" %% "lift-util" % "2.6-M4" exclude("ch.qos.logback","logback-classic")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.jscala" %% "jscala-macros" % "0.4-SNAPSHOT"
