scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-Xlint",
  "-deprecation",
  "-Xfatal-warnings",
  "-feature"
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.4"
resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
libraryDependencies += "org.http4s" %% "http4s-dsl"          % "0.10.1"
libraryDependencies += "org.http4s" %% "http4s-blaze-server" % "0.10.1"
libraryDependencies += "org.http4s" %% "http4s-blaze-client" % "0.10.1"
resolvers ++= Seq(
    "tpolecat" at "http://dl.bintray.com/tpolecat/maven",
      "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
    )
libraryDependencies += "org.tpolecat" %% "doobie-core" % "0.2.3-RC4"
resolvers += Resolver.sonatypeRepo("public")
libraryDependencies += "com.h2database" % "h2" % "1.4.190"
