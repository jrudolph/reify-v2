import sbt._
import sbt.Keys._

object ReifyBuild extends Build {
  lazy val root =
    Project("root", file("."))
      .aggregate(reify, reifyTest)

  lazy val reify =
    Project("reify-v2", file("reify"))
      .settings(defaultSettings: _*)
      .settings(
        libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _ % "provided")
      )

  lazy val reifyTest =
    Project("reify-test", file("reify-test"))
      .settings(defaultSettings: _*)
      .settings(
        publishArtifact := false,
        libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _ % "provided"),
        libraryDependencies ++= Seq(
          "org.specs2" %%  "specs2-core" % "3.7" % "test"
        )
      )
      .dependsOn(reify)

  def defaultSettings = Seq(
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-language:_", "-deprecation", "-unchecked"),

    organization := "net.virtual-void",
    version := "3-SNAPSHOT",
    homepage := Some(url("http://github.com/jrudolph/reify-v2")),
    licenses in GlobalScope += "Apache License 2.0" -> url("https://github.com/jrudolph/reify-v2/raw/master/LICENSE")
) ++ ScalariformSupport.formatSettings
}