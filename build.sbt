ThisBuild / version := "0.1.0-SNAPSHOT"
libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
testFrameworks += new TestFramework("munit.Framework")
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test

ThisBuild / scalaVersion := "3.4.2"

lazy val root = (project in file("."))
  .settings(
    name := "basics_of_translations_methods"
  )
