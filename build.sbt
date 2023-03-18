val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sider",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "io.netty" % "netty-all" % "4.1.89.Final",

    libraryDependencies += "co.fs2" %% "fs2-core" % "3.6.1",
    // optional I/O library
    libraryDependencies += "co.fs2" %% "fs2-io" % "3.6.1",

    // optional reactive streams interop
    libraryDependencies += "co.fs2" %% "fs2-reactive-streams" % "3.6.1",

    // optional scodec interop
    libraryDependencies += "co.fs2" %% "fs2-scodec" % "3.6.1",

    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.6",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.5",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.testcontainers" % "testcontainers" % "1.17.6" % Test
  )