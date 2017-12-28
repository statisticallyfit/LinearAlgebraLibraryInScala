addCommandAlias("namaste", "~test-only org.functionalkoans.forscala.Koans")

name := "LinearAlgebraLibraryInScala"

version := "1.0"

scalaVersion := "2.11.11"

traceLevel := -1

logLevel := Level.Info

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= Seq(
     "org.scala-lang" % "scala-reflect" % "2.11.11",
     "org.apache.commons" % "commons-lang3" % "3.6",
     "org.specs2" % "specs2-core_2.11" % "4.0.1" % "test")


