val scala3Version = "3.6.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sider",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.16",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.15",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.3" % Test,
    libraryDependencies += "org.testcontainers" % "testcontainers" % "1.20.4" % Test
  )