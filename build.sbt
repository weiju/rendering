name := "raytracer"

version := "1.0"

organization := "com.boxofrats"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq("org.jblas" % "jblas" % "1.2.4",
  "com.typesafe.play" %% "play-json" % "2.3.10",
  "org.scalatest" %% "scalatest" % "3.0.0-M16-SNAP6" % "test",
  "junit" % "junit" % "4.12" % "test")

resolvers += Resolver.mavenLocal
