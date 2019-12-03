This is an extremely naïve Scala port of the
[grisu.net](https://github.com/kring/grisu.net.git) library for
rendering floating-point numbers.

It is published on Maven central, so setting up SBT is as simple as

```scala
libraryDependencies += "com.rojoma" %% "grisu-scala" % "1.0.0"
```

While for Maven, the pom snippet is:

```xml
<dependencies>
  <dependency>
    <groupId>com.rojoma</groupId>
    <artifactId>grisu-scala_${scala.version}</artifactId>
    <version>1.0.0</version>
  </dependency>
</dependencies>
```

grisu-scala is published for Scala version 2.10, 2.11, 2.12, and 2.13.
