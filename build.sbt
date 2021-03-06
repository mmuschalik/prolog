val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scalameta" % "scalameta_2.13" % "4.2.3",
    libraryDependencies += "dev.zio" % "zio_2.13" % "1.0.0-RC14",
    libraryDependencies += "dev.zio" % "zio-streams_2.13" % "1.0.0-RC14",
    libraryDependencies += "com.github.scopt" % "scopt_2.13" % "4.0.0-RC2"
  )
