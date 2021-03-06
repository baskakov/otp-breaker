name := """otp-break"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.webjars" % "webjars-play_2.11" % "2.3.0-2",
  "org.webjars" % "bootstrap" % "3.1.0"
)
