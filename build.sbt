val Scala3 = "3.3.1"
val Scala2_13 = "2.13.12"

val scalatestVersion = "3.2.17"
val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion % Test
val scalaTestFlatSpec =
  "org.scalatest" %% "scalatest-flatspec" % scalatestVersion % Test

val scalameta = "org.scalameta" %% "munit" % "0.7.29" % Test

val catsVersion = "2.9.0"
val catsCore = "org.typelevel" %% "cats-core" % catsVersion

val circeVersion = "0.14.6"
val circeCore = "io.circe" %% "circe-core" % circeVersion

val playJsonVersion = "3.0.1"
val playJson = "org.playframework" %% "play-json" % playJsonVersion

// skip / publish := true
ThisBuild / version := "0.1"
// Default to same as circe or SBT isn't happy.
// https://github.com/sbt/sbt/issues/3465
ThisBuild / scalaVersion := Scala2_13
ThisBuild / crossScalaVersions := List(Scala2_13, Scala3)
ThisBuild / organization := "com.quincyjo"
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

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    scalameta,
    scalaTest,
    scalaTestFlatSpec,
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
  .aggregate(core, circe, play)

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "Scala Jsonpath",
    moduleName := "scala-json-path",
    commonSettings
  )

lazy val circe = project
  .in(file("modules/circe"))
  .dependsOn(core)
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
  .dependsOn(core)
  .settings(
    name := "Scala Jsonpath Play",
    moduleName := "scala-json-path-play",
    commonSettings,
    libraryDependencies += playJson
  )
