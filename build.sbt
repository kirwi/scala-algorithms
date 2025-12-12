val scala3Version = "3.3.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-algorithms",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test
    )
  )
