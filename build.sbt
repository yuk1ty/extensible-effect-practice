name := "extensible_effect_practice"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "com.geirsson" %% "funsuite" % "0.1.4"
testFrameworks += new TestFramework("funsuite.Framework")

scalacOptions ++= Seq(
  "-language:higherKinds"
)
