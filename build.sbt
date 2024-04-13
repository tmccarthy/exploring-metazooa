name := "exploring-metazooa"

ThisBuild / tlBaseVersion := "0.0"

//Sonatype.SonatypeKeys.sonatypeProfileName := "au.id.tmm"
ThisBuild / organization := "au.id.tmm.exploring-metazooa"
ThisBuild / organizationName := "Timothy McCarthy"
ThisBuild / startYear := Some(2024)
ThisBuild / developers := List(
  tlGitHubDev("tmccarthy", "Timothy McCarthy"),
)

val Scala213 = "2.13.13"
val Scala3   = "3.2.1"
ThisBuild / scalaVersion := Scala3
ThisBuild / crossScalaVersions := Seq(
//  Scala3,
  Scala213,
)

ThisBuild / githubWorkflowJavaVersions := List(
  JavaSpec.temurin("17"),
)

ThisBuild / tlCiHeaderCheck := false
ThisBuild / tlCiScalafmtCheck := true
ThisBuild / tlCiMimaBinaryIssueCheck := false
ThisBuild / tlFatalWarnings := true

addCommandAlias("check", ";githubWorkflowCheck;scalafmtSbtCheck;+scalafmtCheckAll;+test")
addCommandAlias("fix", ";githubWorkflowGenerate;+scalafmtSbt;+scalafmtAll")

val circeVersion          = "0.14.3"
val tmmUtilsVersion       = "0.10.0"
val tmmCollectionsVersion = "0.2.0"
val sttpVersion           = "3.5.2"
val catsEffectVersion     = "3.2.9"
val slf4jVersion          = "2.0.0-alpha1"
val fetchVersion          = "0.9.1"
val mUnitVersion          = "0.7.27"

lazy val root = tlCrossRootProject
  .settings(console := (core / Compile / console).value)
  .aggregate(
    core,
  )

lazy val core = project
  .in(file("core"))
  .settings(name := "exploring-metazooa-core")
  .enablePlugins(NoPublishPlugin)
  .settings(
    scalacOptions += "-language:implicitConversions",
  )
  .settings(
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-errors"           % tmmUtilsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-cats"             % tmmUtilsVersion,
    libraryDependencies += "au.id.tmm.fetch"                 %% "fetch-core"                 % fetchVersion,
    libraryDependencies += "au.id.tmm.fetch"                 %% "fetch-cache"                % fetchVersion,
    libraryDependencies += "au.id.tmm.tmm-scala-collections" %% "tmm-scala-collections-core" % tmmCollectionsVersion,
    libraryDependencies += "au.id.tmm.tmm-scala-collections" %% "tmm-scala-collections-cats" % tmmCollectionsVersion,
    libraryDependencies += "org.typelevel"                   %% "cats-effect"                % catsEffectVersion,
    libraryDependencies += "org.slf4j"                        % "slf4j-api"                  % slf4jVersion,
  )
  .settings(
    testFrameworks += new TestFramework("munit.Framework"),
    libraryDependencies += "org.scalameta"       %% "munit"                  % mUnitVersion    % Test,
    libraryDependencies += "au.id.tmm.tmm-utils" %% "tmm-utils-testing-core" % tmmUtilsVersion % Test,
    libraryDependencies += "org.typelevel"       %% "munit-cats-effect-3"    % "1.0.5"         % Test,
  )
