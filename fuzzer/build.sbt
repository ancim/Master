name := "fuzzer"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.12",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.12" % Test
)
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.12"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.5.0"
