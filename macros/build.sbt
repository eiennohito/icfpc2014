name := "macros"

scalaVersion := "2.11.2"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
