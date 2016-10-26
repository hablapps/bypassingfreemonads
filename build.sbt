name := "hablapps"

scalaVersion := "2.11.8"

unmanagedSourceDirectories in Test ++= Seq(
  baseDirectory.value
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.0",
  "org.scalatest" %% "scalatest" % "2.2.6"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:postfixOps",
  "-language:higherKinds")

initialCommands in console := """
  |import org.hablapps.talk.bypassingfreemonads._
  |""".stripMargin
