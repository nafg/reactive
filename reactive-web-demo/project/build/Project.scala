import sbt._

class ReactiveWebDemoProject(info: ProjectInfo) extends DefaultWebProject(info) {
  val mavenLocal = "Local Maven Repository" at
  "file://"+Path.userHome+"/.m2/repository"

   val scalatools_snapshot = "Scala Tools Snapshot" at
  "http://scala-tools.org/repo-snapshots/"

  val scalatools_release = "Scala Tools Release" at
  "http://scala-tools.org/repo-releases/"

  val liftVersion = "2.4-SNAPSHOT"

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default" withSources,
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default",
    "junit" % "junit" % "4.5" % "test->default",
    "org.scala-tools.testing" %% "specs" % "1.6.6" % "test->default"
  ) ++ super.libraryDependencies
  
  override def scanDirectories = Nil
}
