import _root_.sbt.Keys._

name := "s-99"

version := "1.0"

scalaVersion := "2.11.1"

resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.1.6" % "test",
  "org.reactivemongo" %% "reactivemongo" % "0.10.5.0.akka23"
)
