javacOptions ++= Seq("-source", "1.11", "-target", "1.11", "-Xlint")

lazy val commonSettings = Seq(
  organization := "nl.kevinvandervlist",
  name := "aoc2020",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.13.3",
  scalacOptions ++= Seq(
    "-feature",
    // "-Werror", disabled for now
    "-deprecation",
    "-unchecked",
    "-Wdead-code",
    "-Wunused:imports,patvars,privates,locals,imports,explicits,implicits,params,linted",
    "-Xlint:deprecation"
  ),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.2" % "test"
  )
)

lazy val puzzles = (project in file("puzzles")).
  settings(commonSettings: _*).
  settings(
    name := "puzzles",
    libraryDependencies ++= Seq(
    ),
  )