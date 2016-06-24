organization := "ucl"

name := "Scenario Week 2016 at UCL CS"

version := "1.0.0 -- SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5"

libraryDependencies += "com.madgag" %% "scala-io-file" % "0.4.2"

libraryDependencies += "org.apache.commons" % "commons-email" % "1.4"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

// mainClass in (Compile, run) := Some("ucl.scenario.geometry.runners.ArtGalleryPainter")

// Server development
libraryDependencies += "com.typesafe.slick" %% "slick" % "3.1.1"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.3.9"
  val sprayV = "1.3.3"
  Seq(
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"            %%  "spray-testkit" % sprayV  % "test",
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
    "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test"
  )
}

Revolver.settings


