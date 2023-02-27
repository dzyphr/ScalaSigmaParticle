scalaVersion := "2.12.16"

name := "name-here"
organization := "org.here"
version := "0.0.1"


ThisBuild / version      := "0.0.1"

// Note, it's not required for you to define these three settings. These are
// mostly only necessary if you intend to publish your library's binaries on a
// place like Sonatype.


// Want to use a published library in your project?
// You can define other libraries as dependencies in your build like this:

libraryDependencies ++= Seq(
  "org.ergoplatform" %% "ergo-appkit" % "5.0.0"
)

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Bintray" at "https://jcenter.bintray.com/"
)
assemblyMergeStrategy in assembly := {
  case "logback.xml" => MergeStrategy.first
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case PathList("org", "bouncycastle", xs @ _*) => MergeStrategy.first
  case PathList("org", "iq80", "leveldb", xs @ _*) => MergeStrategy.first
  case PathList("org", "bouncycastle", xs @ _*) => MergeStrategy.first
  case PathList("javax", "activation", xs @ _*) => MergeStrategy.last
  case PathList("javax", "annotation", xs @ _*) => MergeStrategy.last
  case other => (assemblyMergeStrategy in assembly).value(other)
}

//assemblyJarName in assembly := s"project-name-${version.value}.jar"
//assemblyOutputPath in assembly := file(s"./project-name-${version.value}.jar/")
