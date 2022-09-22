val common = List(
  scalaVersion := "2.13.8"
)

lazy val lang = project
  .in(file("lang"))
  .settings(
    name := "lang",
    version := "0.1.0"
  ).settings(common)