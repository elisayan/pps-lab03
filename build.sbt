val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pps-lab03",
    scalaVersion := scala3Version,
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test
  )
