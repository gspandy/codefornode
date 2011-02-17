import java.io.File
import sbt._

class Project(info: ProjectInfo) extends sbt.DefaultProject(info) {
  self =>
  this

  lazy val m2repo = DefaultMavenRepository

  override def managedStyle = ManagedStyle.Maven

  // Add Maven Local repository for SBT to search for (disable if this doesn't suit you)
  val mavenLocal = "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"
  val publishTo = mavenLocal
  val sourceArtifact = Artifact.sources(artifactID)
  val docsArtifact = Artifact.javadoc(artifactID)

  Resolver.file("Local Maven repository", new File(Path.userHome + "/.m2/repository"))

  val junit = "junit" % "junit" % "4.8.2" % "test"
  val gen = "com.porpoise" % "gen" % "1.0.0-SNAPSHOT"
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
}