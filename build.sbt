val scala3Version = "3.3.1"

val scalatestVersion = "3.2.17"
val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion % Test
val scalaTestFlatSpec =
  "org.scalatest" %% "scalatest-flatspec" % scalatestVersion % Test

val scalameta = "org.scalameta" %% "munit" % "0.7.29" % Test

val catsVersion = "2.10.0"
val catsCore = "org.typelevel" %% "cats-core" % catsVersion

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-jsonpath",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      scalameta,
      scalaTest,
      scalaTestFlatSpec,
      catsCore
    )
  )
