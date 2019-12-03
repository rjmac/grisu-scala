publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

// Random stuff sonatype wants
pomExtra := (
  <url>http://www.github.com/rjmac/grisu-scala</url>
  <licenses>
    <!-- I do not accept that I have the right to dictate terms of use -->
    <license>
      <name>Public domain</name>
    </license>
  </licenses>
  <scm>
    <url>git://github.com/rjmac/grisu-scala.git</url>
    <connection>scm:git://github.com/rjmac/grisu-scala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>robertm</id>
      <name>Robert Macomber</name>
    </developer>
  </developers>
)

// Random stuff sonatype does not want
pomIncludeRepository := { _ => false }

// These are the defaults, but set them manually just in case

publishMavenStyle := true

publishArtifact in Test := false
