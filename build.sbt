
scalaVersion := "2.11.2"

lazy val marcrotest = project in file(".") dependsOn(macros)

lazy val macros = project

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
