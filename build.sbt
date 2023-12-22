val scala3Version = "3.3.1"
val scala2Version = "2.13.12"

val scalatestVersion = "3.2.17"
val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion % Test
val scalaTestFlatSpec =
  "org.scalatest" %% "scalatest-flatspec" % scalatestVersion % Test

val scalameta = "org.scalameta" %% "munit" % "0.7.29" % Test

val catsVersion = "2.10.0"
val catsCore = "org.typelevel" %% "cats-core" % catsVersion

skip / publish := true
organization := "com.quincyjo"
homepage := Some(url("https://github.com/quincyjo/scala-json-path"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/quincyjo/scala-json-path"),
    "git@github.com:quincyjo/scala-json-path.git"
  )
)
developers := List(
  Developer(
    "quincyjo",
    "Quincy Jo",
    "me@quincyjo.com",
    url("https://github.com/quincyjo")
  )
)
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-jsonpath",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    crossScalaVersions := List(scala3Version, scala2Version),
    libraryDependencies ++= Seq(
      scalameta,
      scalaTest,
      scalaTestFlatSpec,
      catsCore
    )
  )
