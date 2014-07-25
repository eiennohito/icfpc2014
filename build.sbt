
scalaVersion := "2.11.2"

lazy val marcrotest = project in file(".") dependsOn(macros)

lazy val macros = project

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"
