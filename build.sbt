val common = List(
  scalaVersion := "2.13.8"
)

lazy val lang = project
  .in(file("lang"))
  .settings(
    name := "lang",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "2.3.3",
      "org.scalatest" %% "scalatest" % "3.2.15" % "test"
    )
  )
  .settings(common)
