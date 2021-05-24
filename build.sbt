name := "VBPots-V3"

version := "0.1"

scalaVersion := "2.12.12"

retrieveManaged := true

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "au.com.bytecode" % "opencsv" % "2.4"
libraryDependencies +=  "com.storm-enroute"  %% "scalameter" % "0.9"

scalacOptions += "-Ydelambdafy:inline"