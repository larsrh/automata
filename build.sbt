name := "afl-assignment"

version := "0.2"

scalaVersion := "2.9.1"

scalacOptions ++= Seq(
	"-Ydependent-method-types",
	"-Xelide-below", "MAXIMUM",
	"-deprecation",
	"-unchecked"
)

libraryDependencies +=
	"org.scalaz" %% "scalaz-core" % "6.0.3"

mainClass := Some("edu.tum.cs.afl.Launcher")

seq(assemblySettings: _*)

seq(antlrSettings: _*)
