import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

name := "grisu-scala"

organization := "com.rojoma"

version := "1.0.0"

previousArtifact := None // Some("com.rojoma" % ("grisu-scala_" + scalaBinaryVersion.value) % "1.0.0")

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.10.4", "2.11.6")

libraryDependencies ++= Seq(
    "org.spire-math" %% "spire" % "0.9.1",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )

scalacOptions ++= Seq("-deprecation", "-feature", "-optimize")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
sourceGenerators in Compile <+= (baseDirectory, version, crossScalaVersions) map READMEBuilder
