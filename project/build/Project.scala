import sbt._

class LiftReactive(info: ProjectInfo) extends DefaultProject(info) {
  val mavenLocal = "Local Maven Repository" at
  "file://"+Path.userHome+"/.m2/repository"

   val scalatools_snapshot = "Scala Tools Snapshot" at
  "http://scala-tools.org/repo-snapshots/"

  val scalatools_release = "Scala Tools Release" at
  "http://scala-tools.org/repo-releases/"


  override def libraryDependencies = Set(
    "junit" % "junit" % "4.5" % "test->default",
    "org.scala-tools.testing" %% "specs" % "1.6.5" % "test->default" withSources,
    "org.scalatest" % "scalatest" % "1.2" % "test->default" withSources
  ) ++ super.libraryDependencies

}
