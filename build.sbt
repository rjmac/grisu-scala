name := "grisu-scala"

organization := "com.rojoma"

version := "1.0.0"

mimaPreviousArtifacts := Set(/* "com.rojoma" % ("grisu-scala_" + scalaBinaryVersion.value) % "1.0.0" */)

scalaVersion := "2.13.1"

crossScalaVersions := Seq("2.10.4", "2.11.6", "2.12.10", scalaVersion.value)

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  )

scalacOptions ++= Seq("-deprecation", "-feature") ++ {
  scalaVersion.value match {
    case "2.12.10" | "2.13.1" => Seq("-opt:l:inline", "-opt-inline-from:**")
    case "2.10.4" | "2.11.6" => Seq("-optimize")
  }
}

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
sourceGenerators in Compile += Def.task { READMEBuilder(baseDirectory.value, version.value, crossScalaVersions.value) }
