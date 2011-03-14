import java.io.File
import sbt._
import reaktor.scct.ScctProject

class Project(info: ProjectInfo) extends DefaultProject(info) with ScctProject {
  self => this

  lazy val m2repo = DefaultMavenRepository

  override def managedStyle = ManagedStyle.Maven

  // Add Maven Local repository for SBT to search for (disable if this doesn't suit you)
  val mavenLocal = "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"
  val publishTo = mavenLocal
  val sourceArtifact = Artifact.sources(artifactID)
  val docsArtifact = Artifact.javadoc(artifactID)

  Resolver.file("Local Maven repository", new File(Path.userHome + "/.m2/repository"))

  val junit = "junit" % "junit" % "4.8.2" % "test"
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  val ui =  <dependency><groupId>com.porpoise</groupId>
  <artifactId>codefornode-ui</artifactId>
  <version>0.0.1-SNAPSHOT</version></dependency>

  //val gen = "com.porpoise" % "gen" % "1.0.0-SNAPSHOT"
}