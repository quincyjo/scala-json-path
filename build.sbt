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

val braidVersion = "0.1.1"
val braid = "com.quincyjo" %% "braid" % braidVersion
val braidOperations = "com.quincyjo" %% "braid-json-operations" % braidVersion
val braidCirce = "com.quincyjo" %% "braid-circe" % braidVersion
val braidPlay = "com.quincyjo" %% "braid-play" % braidVersion

// skip / publish := true
ThisBuild / tlBaseVersion := "0.2"
ThisBuild / version := "0.2.0"
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
ThisBuild / tlJdkRelease := Some(17)

Global / excludeLintKeys += tlBaseVersion

Global / initialCommands :=
  """import com.quincyjo.jsonpath._
    |import com.quincyjo.jsonpath.literal._
    |import com.quincyjo.jsonpath.JsonPath._
    |import com.quincyjo.jsonpath.Expression._
    |import com.quincyjo.jsonpath.extensions._
    |import com.quincyjo.jsonpath.parser.JsonPathParser
    |""".stripMargin

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    scalameta % Test,
    scalaTest % Test,
    scalaTestFlatSpec % Test,
    catsCore,
    braid,
    braidOperations
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
  .dependsOn(core, testBehaviours % Test)
  .settings(
    name := "Scala Jsonpath Circe",
    moduleName := "scala-json-path-circe",
    tlVersionIntroduced := Map("3" -> "0.2.0"),
    libraryDependencies ++= Seq(circeCore, braidCirce)
  )

lazy val play = project
  .in(file("modules/play"))
  .dependsOn(core, testBehaviours % Test)
  .settings(
    name := "Scala Jsonpath Play",
    moduleName := "scala-json-path-play",
    commonSettings,
    libraryDependencies ++= Seq(playJson, braidPlay)
  )

lazy val testBehaviours = project
  .in(file("modules/test-behaviours"))
  .dependsOn(core)
  .settings(
    skip := true,
    publish / skip := true,
    update / skip := false,
    compile / skip := false,
    libraryDependencies ++= Seq(
      scalameta,
      scalaTest,
      scalaTestFlatSpec
    )
  )
