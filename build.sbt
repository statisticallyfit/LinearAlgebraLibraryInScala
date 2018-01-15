//addCommandAlias("namaste", "~test-only org.functionalkoans.forscala.Koans")

name := "LinearAlgebraLibraryInScala"

version := "1.0"

scalaVersion := "2.11.11"

traceLevel := -1

logLevel := Level.Info

// append -deprecation to the options passed to the Scala compiler

//add this feature "-Ypartial-unification" for scala 2.11.9 or later for
//improved type inference in Cats
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Ypartial-unification")

libraryDependencies ++= Seq(
     //Scala Reflection
     "org.scala-lang" % "scala-reflect" % "2.11.11",

     //Apache (Java)
     "org.apache.commons" % "commons-lang3" % "3.6",

     // Scala Specs2 testing
     "org.specs2" % "specs2-core_2.11" % "4.0.1" % "test",

     //Spire
     "org.typelevel" %% "spire" % "0.14.1",

     // Scalaz
     "org.scalaz"      %% "scalaz-core"    % "7.3.0-M19",

     // Cats: https://typelevel.org/cats/
     "org.typelevel"   %% "cats-core"           % "1.0.1",
     "org.typelevel"   %% "cats-macros"           % "1.0.1",
     "org.typelevel"   %% "cats-kernel"           % "1.0.1",
     "org.typelevel"   %% "cats-laws"           % "1.0.1",
     "org.typelevel"   %% "cats-free"           % "1.0.1",
     "org.typelevel"   %% "cats-testkit"           % "1.0.1",

     //Shapeless
     "com.chuusai"     %% "shapeless"      % "2.3.3")


