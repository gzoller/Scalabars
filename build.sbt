import Dependencies._
import scoverage.ScoverageKeys._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val graalversion = "19.1.0"

val fastparse    = "com.lihaoyi"          %% "fastparse"      % "2.1.3"
val commonsText  = "org.apache.commons"   % "commons-text"    % "1.6"
val json4s       = "org.json4s"           %% "json4s-core"    % "3.6.5"
val json4sNative = "org.json4s"           %% "json4s-native"  % "3.6.5"
val scalajack    = "co.blocke"            %% "scalajack"      % "6.0.1"
val graalvm      = "org.graalvm.sdk"      % "graal-sdk"       % graalversion
val graaljs      = "org.graalvm.js"       % "js-scriptengine" % graalversion
val graaljs2     = "org.graalvm.js"       % "js"              % graalversion
val markdown     = "com.vladsch.flexmark" % "flexmark-all"    % "0.42.6"
val zipper       = "co.blocke"            %% "listzipper"     % "0.1.3"

val basicSettings = Seq(
  coverageMinimum := 92, // really this should be 96% but mongo isn't quite up to that yet
  coverageFailOnMinimum := true,
  parallelExecution := false,
  scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
  Test / parallelExecution := false,
  resolvers += "Bintray Releases" at "http://dl.bintray.com/blocke/releases/"
)

lazy val root = (project in file("."))
  .settings(basicSettings: _*)
  .settings(
    name := "scalabars",
    libraryDependencies ++= Seq(fastparse,
                                zipper,
                                commonsText,
                                scalajack,
                                graalvm,
                                graaljs,
                                graaljs2,
                                json4s,
                                json4sNative,
                                markdown,
                                scalaTest % Test)
  )
