ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "compiler"
  )

libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.0.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % "test"
libraryDependencies += "org.scala-lang.modules" % "scala-asm" % "9.5.0-scala-1"
