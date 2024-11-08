ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.2"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M7" % Test


lazy val root = (project in file("."))
  .settings(
    name := "basics_of_translations_methods"
  )
