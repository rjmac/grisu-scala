name := "grisu-scala"

organization := "com.rojoma"

version := "1.0.0"

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.10.4", "2.11.6")

libraryDependencies ++= Seq(
    "org.spire-math" %% "spire" % "0.9.1",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )

scalacOptions += "-deprecation"
