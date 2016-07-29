name := "matrix-scala-sdk"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "io.spray"          %% "spray-json" % "1.3.2",
  "com.typesafe.akka" %% "akka-actor" % "2.4.3",
  "com.typesafe.akka" %% "akka-http-core" % "2.4.3",
  "com.typesafe.akka" %% "akka-http-testkit" % "2.4.3",
  "com.typesafe.akka" %% "akka-http-experimental" % "2.4.3",
  "com.beachape" %% "enumeratum" % "1.4.8",
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % "2.4.3"

)