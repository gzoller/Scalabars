import Dependencies._
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._
import scoverage.ScoverageKeys._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val fastparse    = "com.lihaoyi"        %% "fastparse"      % "2.1.0"
val commonsText  = "org.apache.commons" % "commons-text"    % "1.6"
val json4s       = "org.json4s"         %% "json4s-core"    % "3.6.5"
val json4sNative = "org.json4s"         %% "json4s-native"  % "3.6.5"
val scalajack    = "co.blocke"          %% "scalajack"      % "6.0.1"
val graalvm      = "org.graalvm.sdk"    % "graal-sdk"       % "1.0.0-rc16"
val graaljs      = "org.graalvm.js"     % "js-scriptengine" % "1.0.0-rc16"
val graaljs2     = "org.graalvm.js"     % "js"              % "1.0.0-rc16"

val basicSettings = Seq(
  javacOptions ++= Seq("-Xlint:-removal"),
  coverageMinimum             := 92,  // really this should be 96% but mongo isn't quite up to that yet
  coverageFailOnMinimum       := true,
  parallelExecution in ThisBuild := false,
//  javacOptions ++= Seq("-target", "1.8.0_202"),
//  scalacOptions += "-target:1.8.0_202",
  resolvers += "Bintray Releases" at "http://dl.bintray.com/blocke/releases/",
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(AlignArguments, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentConstructorArguments, true),
)

lazy val root = (project in file("."))
  .settings(basicSettings: _*)
  .settings(
    name := "scalabars",
    libraryDependencies ++= Seq(fastparse, graaljs, graaljs2, commonsText, scalajack, json4s, json4sNative, scalaTest % Test)
  )