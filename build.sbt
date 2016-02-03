libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.9" % "test"
)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _ % "provided")

scalaVersion := "2.11.7"

ScalariformSupport.formatSettings

scalacOptions ++= Seq("-language:_", "-deprecation", "-unchecked")