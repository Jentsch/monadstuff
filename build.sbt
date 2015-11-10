name := "monadstuff"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.5"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"

// scalacOptions in console += "-Xlog-implicits"

initialCommands := "import scalaz._; import Scalaz._"
