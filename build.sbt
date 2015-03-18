name := """ga4gh-server"""

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

resolvers += "BBOP snapshots" at "http://knife.lbl.gov/maven/repository/"

resolvers += "BBOP" at "http://knife.lbl.gov/maven/snapshot-repository/"