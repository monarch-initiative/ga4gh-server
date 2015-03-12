name := """g2p"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws
)

libraryDependencies += "org.ga4gh" % "ga4gh-format" % "0.6.0-SNAPSHOT"

resolvers += Resolver.mavenLocal