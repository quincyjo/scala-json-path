val Scala3 = "3.3.1"
val Scala2_13 = "2.13.12"

val scalatestVersion = "3.2.17"
val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion
val scalaTestFlatSpec =
  "org.scalatest" %% "scalatest-flatspec" % scalatestVersion

val scalameta = "org.scalameta" %% "munit" % "0.7.29"

val catsVersion = "2.9.0"
val catsCore = "org.typelevel" %% "cats-core" % catsVersion

val circeVersion = "0.14.6"
val circeCore = "io.circe" %% "circe-core" % circeVersion

val playJsonVersion = "3.0.1"
val playJson = "org.playframework" %% "play-json" % playJsonVersion

// skip / publish := true
ThisBuild / tlBaseVersion := "0.1"
ThisBuild / version := "0.1.0"
// Default to same as circe or SBT isn't happy.
// https://github.com/sbt/sbt/issues/3465
ThisBuild / scalaVersion := Scala2_13
ThisBuild / crossScalaVersions := List(Scala2_13, Scala3)
ThisBuild / organization := "com.quincyjo"
ThisBuild / organizationName := "Quincy Jo"
ThisBuild / organizationHomepage := Some(url("https://quincyjo.com"))
ThisBuild / homepage := Some(url("https://github.com/quincyjo/scala-json-path"))
ThisBuild / startYear := Some(2023)
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/quincyjo/scala-json-path"),
    "git@github.com:quincyjo/scala-json-path.git"
  )
)
ThisBuild / developers := List(
  Developer(
    "quincyjo",
    "Quincy Jo",
    "me@quincyjo.com",
    url("https://github.com/quincyjo")
  )
)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / tlJdkRelease := Some(11)

Global / excludeLintKeys += tlBaseVersion

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    scalameta % Test,
    scalaTest % Test,
    scalaTestFlatSpec % Test,
    catsCore
  ),
  scalacOptions ++= (if (!tlIsScala3.value)
                       Seq(
                         "-feature",
                         "-language:implicitConversions"
                       )
                     else Seq.empty)
)

lazy val root = tlCrossRootProject
  .aggregate(core, circe, play, testBehaviours)

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "Scala Jsonpath",
    moduleName := "scala-json-path",
    commonSettings
  )

lazy val circe = project
  .in(file("modules/circe"))
  .dependsOn(core, testBehaviours % Test)
  .settings(
    name := "Scala Jsonpath Circe",
    moduleName := "scala-json-path-circe",
    skip := tlIsScala3.value,
    update / skip := false,
    libraryDependencies ++= (
      if (tlIsScala3.value) Nil
      else
        Seq(
          scalameta,
          scalaTest,
          scalaTestFlatSpec,
          circeCore
        )
    )
  )

lazy val play = project
  .in(file("modules/play"))
  .dependsOn(core, testBehaviours % Test)
  .settings(
    name := "Scala Jsonpath Play",
    moduleName := "scala-json-path-play",
    commonSettings,
    libraryDependencies += playJson
  )

lazy val testBehaviours = project
  .in(file("modules/test-behaviours"))
  .dependsOn(core)
  .settings(
    publish / skip := true,
    update / skip := false,
    libraryDependencies ++= Seq(
      scalameta,
      scalaTest,
      scalaTestFlatSpec
    )
  )
