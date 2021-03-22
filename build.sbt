name := "scala-with-cats"

version := "0.1"

scalaVersion := "2.12.13"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "1.0.0"
scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)