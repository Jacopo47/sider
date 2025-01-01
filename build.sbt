val scala3Version = "3.6.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sider",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.6",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.5",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.testcontainers" % "testcontainers" % "1.17.6" % Test
  )