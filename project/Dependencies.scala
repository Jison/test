import sbt._

object Dependencies {

  lazy val akkaVersion = "2.4.9"

  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion
  val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test
  val logback = "ch.qos.logback" % "logback-classic" % "1.1.7"

  val apacheThrift = "org.apache.thrift" % "libthrift" % "0.9.3"

  val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

  val scalafmt = "com.geirsson" %% "scalafmt-core" % "0.2.3"
  val scopt = "com.github.scopt" %% "scopt" % "3.3.0"

  val scalatest = "org.scalatest" %% "scalatest" % "2.2.6" % Test

}