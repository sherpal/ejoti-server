ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

ThisBuild / scalacOptions ++= List(
  "-encoding", "utf8", // if an option takes an arg, supply it on the same line
  "-feature", // then put the next option on a new line for easy editing
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  "-Yexplicit-nulls"
)

val circeVersion = "0.14.1"
ThisBuild / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val Ejoti = crossProject(JSPlatform, JVMPlatform).in(file("./ejoti"))
  .settings(
    name := "Ejoti",
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    libraryDependencies ++= List(
      "dev.zio" %%% "zio" % "2.0.5",
      "dev.zio" %%% "zio-streams" % "2.0.5",
      "be.doeraene" %%% "url-dsl" % "0.5.0",
      "io.github.cquiroz" %%% "scala-java-time" % "2.4.0"
    ) ++ List(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion) ++ List(
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
      "org.scalacheck" %%% "scalacheck" % "1.17.0" % Test,
      "dev.zio" %%% "zio-test" % "2.0.6" % Test,
      "dev.zio" %%% "zio-test-sbt"      % "2.0.6" % Test
    )
  )

lazy val server = (project in file("./server"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "server",
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule))
  )
  .dependsOn(Ejoti.js)
