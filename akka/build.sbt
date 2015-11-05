name := "kmeans"

organization := "eu.unicredit"

scalaVersion := "2.11.7"

fork in run := true

libraryDependencies ++=  Seq(
  "org.json4s" %% "json4s-jackson" % "3.2.11",
  "com.typesafe.akka" %% "akka-actor" % "2.4.0"
)
