name := "reactive-web"

description := "FRP-based abstractions to control the browser from the server"

libraryDependencies += "net.liftweb" %% "lift-util" % "2.6" exclude("ch.qos.logback","logback-classic")

libraryDependencies += "com.lihaoyi" %% "upickle" % "0.2.8"
