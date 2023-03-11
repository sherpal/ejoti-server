ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.2.0"

ThisBuild / scalacOptions ++= List(
  "-encoding",
  "utf8", // if an option takes an arg, supply it on the same line
  "-feature", // then put the next option on a new line for easy editing
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  "-Yexplicit-nulls"
)

val circeVersion = "0.14.1"
val zioVersion   = "2.0.9"

ThisBuild / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val Ejoti = crossProject(JSPlatform, JVMPlatform)
  .in(file("./ejoti"))
  .settings(
    name := "Ejoti",
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    libraryDependencies ++= List(
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion,
      "dev.zio" %%% "zio-interop-cats" % "23.0.0.1",
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
      "dev.zio" %%% "zio-test-sbt" % "2.0.6" % Test
    )
  )
  .jsSettings(
    resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
    libraryDependencies ++= List(
      "co.fs2" %%% "fs2-io" % "3.5.0-43-87fccf7-SNAPSHOT"
    )
  )
  .jvmSettings(
    libraryDependencies ++= List(
      "co.fs2" %% "fs2-io" % "3.5.0"
    )
  )

lazy val server = (project in file("./server"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "server",
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    publish / skip := true
  )
  .dependsOn(Ejoti.js)

val createReleaseTag = taskKey[java.io.File]("Writes the current release tag in tag.txt file")

createReleaseTag := {
  val file = new java.io.File("tag.txt")

  val currentVersion = (`Ejoti`.jvm / version).value
  val gitHash        = git.gitHeadCommit.value.toRight(new IllegalStateException("Not a git repo!")).toTry.get.take(8)
  IO.write(file, s"RELEASE_TAG=$currentVersion-$gitHash")

  file
}
