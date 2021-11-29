javacOptions ++= Seq("-source", "1.11", "-target", "1.11", "-Xlint")

lazy val commonSettings = Seq(
  organization := "nl.kevinvandervlist",
  name := "Advent of Code",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "3.1.0",
  scalacOptions ++= Seq(
    "-feature",
    // "-Werror", disabled for now
    "-deprecation",
    "-unchecked",
    //"-Wdead-code",
    //"-Wunused:imports,patvars,privates,locals,implicits,params,linted",
    //"-Xlint:deprecation"
  ),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )
)

lazy val core = (project in file("core")).
  settings(commonSettings: _*).
  settings(
    name := "core",
    libraryDependencies ++= Seq(
    ),
  )

lazy val y2015 = (project in file("y2015")).
  settings(commonSettings: _*).
  settings(
    name := "y2015",
    libraryDependencies ++= Seq(
    ),
  ).dependsOn(core % "compile->compile;test->test")

lazy val y2016 = (project in file("y2016")).
  settings(commonSettings: _*).
  settings(
    name := "y2016",
    libraryDependencies ++= Seq(
    ),
  ).dependsOn(core % "compile->compile;test->test")

lazy val y2017 = (project in file("y2017")).
  settings(commonSettings: _*).
  settings(
    name := "y2017",
    libraryDependencies ++= Seq(
    ),
  ).dependsOn(core % "compile->compile;test->test")

lazy val y2018 = (project in file("y2018")).
  settings(commonSettings: _*).
  settings(
    name := "y2018",
    libraryDependencies ++= Seq(
    ),
  ).dependsOn(core % "compile->compile;test->test")

lazy val y2019 = (project in file("y2019")).
  settings(commonSettings: _*).
  settings(
    name := "y2019",
    libraryDependencies ++= Seq(
    ),
  ).dependsOn(core % "compile->compile;test->test")

lazy val y2020 = (project in file("y2020")).
  settings(commonSettings: _*).
  settings(
    name := "y2020",
    libraryDependencies ++= Seq(
    ),
  ).dependsOn(core % "compile->compile;test->test")

lazy val y2021 = (project in file("y2021")).
  settings(commonSettings: _*).
  settings(
    name := "y2021",
    libraryDependencies ++= Seq(
    ),
  ).dependsOn(core % "compile->compile;test->test")
