name := "scala-with-cats"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "2.4.0"
scalacOptions ++= Seq(
  "-Xfatal-warnings"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)