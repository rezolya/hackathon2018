name := "hackathon2018"
organization := "com.ing"
version := "0.1"

scalaVersion := "2.11.11"

libraryDependencies ++= {
  val akkaV = "2.5.11"
  val akkaHttpV = "10.1.0"
  val scalaTestV = "3.0.5"
  val googleCloudVisionV = "1.14.0"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV,
    "org.scalatest" %% "scalatest" % scalaTestV % "test",
    "com.google.cloud" % "google-cloud-vision" % googleCloudVisionV
  )
}
