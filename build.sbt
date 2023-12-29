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

// skip / publish := true
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := Scala2_13
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

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    scalameta,
    scalaTest,
    scalaTestFlatSpec,
    catsCore
  ),
  scalacOptions ++= Seq(
    "-feature",
    "-language:implicitConversions"
  )
)

lazy val root = tlCrossRootProject
  .aggregate(core, circe)

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "Scala Jsonpath",
    moduleName := "scala-json-path",
    commonSettings,
    // Default to same as circe or SBT isn't happy.
    // https://github.com/sbt/sbt/issues/3465
    scalaVersion := Scala2_13,
    crossScalaVersions := List(Scala2_13, Scala3)
  )

lazy val circe = project
  .in(file("modules/circe"))
  .dependsOn(core)
  .settings(
    name := "Scala Jsonpath Circe",
    moduleName := "scala-json-path-circe",
    scalaVersion := Scala2_13,
    commonSettings,
    libraryDependencies += circeCore,
    crossScalaVersions := List(Scala2_13)
  )
