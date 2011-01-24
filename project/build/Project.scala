import sbt._


class Project(info: ProjectInfo) extends ParentProject(info) {
  val mavenLocal = "Local Maven Repository" at
  "file://"+Path.userHome+"/.m2/repository"

   val scalatools_snapshot = "Scala-Tools Snapshot" at
  "http://scala-tools.org/repo-snapshots/"

  val scalatools_release = "Scala-Tools Release" at
  "http://scala-tools.org/repo-releases/"

	lazy val reactive = project("reactive-core")
	lazy val reactive_web = project("reactive-web", reactive)
  lazy val reactive_web_demo = project("reactive-web-demo", reactive_web)
}
