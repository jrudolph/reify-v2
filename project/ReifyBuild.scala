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
          ("org.specs2" %%  "specs2-core" % "3.7" % "test")
            .exclude( "org.scalaz", "scalaz-concurrent_2.12.0-M2" )
            .exclude( "org.scalaz", "scalaz-effect_2.12.0-M2" )
            .exclude( "org.scalaz", "scalaz-core_2.12.0-M2" )
            .exclude( "org.scala-lang.modules", "scala-xml_2.12.0-M2" )
            .exclude( "org.scala-lang.modules", "scala-parser-combinators_2.12.0-M2" )
        )
      )
      .dependsOn(reify)

  def defaultSettings = Seq(
    crossScalaVersions := Seq("2.11.7", "2.12.0-M3"),
    scalacOptions ++= Seq("-language:_", "-deprecation", "-unchecked"),

    organization := "net.virtual-void",
    version := "3-SNAPSHOT",
    homepage := Some(url("http://github.com/jrudolph/reify-v2")),
    licenses in GlobalScope += "Apache License 2.0" -> url("https://github.com/jrudolph/reify-v2/raw/master/LICENSE")
) ++ ScalariformSupport.formatSettings
}