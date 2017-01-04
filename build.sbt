name := "azucar"

organization := "org.hablapps"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.typelevel" %% "kittens" % "1.0.0-M7-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.slamdata" %% "matryoshka-core" % "0.16.1")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")
