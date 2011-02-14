import sbt._
class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val installer = "org.scala-tools.sbt" % "installer-plugin" % "0.3.0"
}
