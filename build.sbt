name := "gondola"

organization := "org.higherState"

version := "1.0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps", "-language:reflectiveCalls",
  "-unchecked",
  "-Xfatal-warnings",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Xfuture"
)
javacOptions ++= Seq("-target", "1.8", "-source", "1.8", "-Xlint:deprecation")

initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8")
    sys.error("Java 8 is required for this project.")
}

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.11.8",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  "org.typelevel" %% "cats" % "0.4.1",
  "com.typesafe.akka" %% "akka-actor" % "2.4.3",
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1" cross CrossVersion.binary)

resolvers ++= Seq (
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
)
