name := "afl-assignment"

version := "0.1"

scalaVersion := "2.9.1"

scalacOptions ++= Seq(
	"-Ydependent-method-types",
	"-deprecation",
	"-unchecked"
)

libraryDependencies +=
	"org.scalaz" %% "scalaz-core" % "6.0.3"
