name := "basel-face-pipeline"

version := "0.1"

scalaVersion := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers += Resolver.jcenterRepo

resolvers += Resolver.bintrayRepo("unibas-gravis", "maven")

libraryDependencies += "ch.unibas.cs.gravis" %% "scalismo-faces" % "0.9.0"

libraryDependencies += "ch.unibas.cs.gravis" %% "scalismo-ui" % "0.11.+"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.3"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"

libraryDependencies ~= { _.map(_.exclude("org.slf4j", "slf4j-nop")) }

