import Dependencies._

lazy val commonSettings = Seq(
  scalaVersion := "2.11.8"
)

lazy val thrift = (project in file("ciyo-thrift"))
  .settings(commonSettings)
  .settings(
    name := "ciyo-thrift",
    version := "1.0",
    libraryDependencies ++= Seq(apacheThrift, scopt, scalafmt, scalaParser, scalatest)
  )

lazy val rpc = (project in file("ciyo-rpc"))
  .settings(commonSettings)
  .settings(
    name := "ciyo-rpc",
    version := "1.0",
    libraryDependencies ++= Seq(akkaActor, akkaStream, akkaSlf4j, akkaTestkit, scalatest, apacheThrift)
  ).dependsOn(thrift)

fork := true
cancelable in Global := true
