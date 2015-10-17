scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-Xlint",
  "-deprecation",
  "-Xfatal-warnings",
  "-feature"
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"
