val common = List(
  scalaVersion := "2.13.8"
)

lazy val lang = project
  .in(file("lang"))
  .settings(
    name := "lang",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.8.1" % "test",
      "com.lihaoyi" %% "fastparse" % "2.3.3"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).settings(common)