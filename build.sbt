import scoverage.ScoverageKeys._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val graalversion = "19.1.0"

val fastparse    = "com.lihaoyi"              %% "fastparse"      % "2.1.3"
val commonsText  = "org.apache.commons"       % "commons-text"    % "1.6"
val json4s       = "org.json4s"               %% "json4s-core"    % "3.6.7"
val json4sNative = "org.json4s"               %% "json4s-native"  % "3.6.7"
val scalajack    = "co.blocke"                %% "scalajack"      % "6.0.3"
val graalvm      = "org.graalvm.sdk"          % "graal-sdk"       % graalversion
val graaljs      = "org.graalvm.js"           % "js-scriptengine" % graalversion
val graaljs2     = "org.graalvm.js"           % "js"              % graalversion
val markdown     = "com.vladsch.flexmark"     % "flexmark-all"    % "0.42.6"
val zipper       = "co.blocke"                %% "listzipper"     % "0.1.3"
val logApi       = "org.apache.logging.log4j" % "log4j-api"       % "2.12.0"
val logCore      = "org.apache.logging.log4j" % "log4j-core"      % "2.12.0"
val scalatest    = "org.scalatest"            %% "scalatest"      % "3.1.0-SNAP13"

lazy val crossVersions = crossScalaVersions := Seq("2.12.8", "2.13.0")

val basicSettings = Seq(
  coverageMinimum := 92, // really this should be 96% but mongo isn't quite up to that yet
  coverageFailOnMinimum := true,
  parallelExecution := false,
  scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
  Test / parallelExecution := false,
  resolvers += "Bintray Releases" at "http://dl.bintray.com/blocke/releases/"
)

val pubSettings = Seq(
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "handlebars")
)

lazy val root = (project in file("."))
  .settings(basicSettings ++ crossVersions: _*)
  .settings(pubSettings: _*)
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
                                logCore,
                                logApi,
                                json4sNative,
                                markdown,
                                scalatest % Test)
  )
