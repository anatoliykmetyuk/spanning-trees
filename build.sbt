val ScalaVer = "2.12.2"

val Cats          = "0.9.0"
val Shapeless     = "2.3.2"
val KindProjector = "0.9.3"
val Scalacheck    = "1.13.5"

val ScalacheckMinTests = 1000

lazy val commonSettings = Seq(
  name    := "Spanning Trees"
, version := "0.1.0"
, scalaVersion := ScalaVer
, libraryDependencies ++= Seq(
    "org.typelevel"  %% "cats"            % Cats
  , "com.chuusai"    %% "shapeless"       % Shapeless
  , "org.scalacheck" %% "scalacheck"      % Scalacheck  % "test"
  )
, addCompilerPlugin("org.spire-math" %% "kind-projector" % KindProjector)
, scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:experimental.macros",
      "-unchecked",
      // "-Xfatal-warnings",
      "-Xlint",
      // "-Yinline-warnings",
      "-Ywarn-dead-code",
      "-Xfuture",
      "-Ypartial-unification")
, testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", ScalacheckMinTests.toString, "-workers", "10", "-verbosity", "1")
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(
    initialCommands := "import spanningtrees._"
  )
