scalaVersion := "2.11.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)
