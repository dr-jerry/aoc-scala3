val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq("com.softwaremill.sttp.client4" %% "core" % "4.0.0-M19",
        "org.scalameta" %% "munit" % "1.0.0" % Test
  ))