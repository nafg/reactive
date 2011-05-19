import sbt._


trait Common {this: DefaultProject =>
  val mavenLocal = "Local Maven Repository" at
  "file://"+Path.userHome+"/.m2/repository"

   val scalatools_snapshot = "Scala-Tools Snapshot" at
  "http://scala-tools.org/repo-snapshots/"

  val scalatools_release = "Scala-Tools Release" at
  "http://scala-tools.org/repo-releases/"

  val sonatype_snapshot = "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
  
  val publishTo = "Sonatype Nexus Repository Manager" at "https://oss.sonatype.org/content/repositories/snapshots"

  Credentials(Path fromFile "/private/nafg/.credentials", log)

}


class Project(info: ProjectInfo) extends ParentProject(info) {
  lazy val reactive = project(
    "reactive-core",
    "reactive-core",
    new DefaultProject(_) with Common {
      override def managedStyle = ManagedStyle.Maven
      override def pomExtra = <xml:group>
        <name>reactive-core</name>
        <description>An FRP framework</description>
        <url>http://wwww.reactive-web.co.cc</url>
        <licenses></licenses>
        <scm>
          <connection>scm:git:git://github.com/nafg/reactive.git</connection>
          <developerConnection>scm:git:git@github.com:nafg/reactive.git</developerConnection>
          <url>git@github.com:nafg/reactive.git</url>
        </scm>
        <developers></developers>
      </xml:group>
      val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test->default" withSources
    }
  )
  lazy val reactive_web = project(
    "reactive-web",
    "reactive-web",
    new DefaultProject(_) with Common {
      override def managedStyle = ManagedStyle.Maven
      override def pomExtra = <xml:group>
        <name>reactive-web</name>
        <description>FRP-based abstractions for Ajax and Comet</description>
        <url>http://wwww.reactive-web.co.cc</url>
        <licenses></licenses>
        <scm><url></url><connection></connection></scm>
        <developers></developers>
      </xml:group>
      val liftVersion = "2.4-SNAPSHOT"
      val testkit = "net.liftweb" %% "lift-testkit" % liftVersion
      val webkit = "net.liftweb" %% "lift-webkit" % liftVersion
      val jetty = "org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default"
      val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test->default" withSources
    },
    reactive)
  lazy val reactive_web_demo = project("reactive-web-demo", reactive_web)
}
