
scalaVersion := "2.13.3"

name := "day-grid"
organization := "io.syspulse"
version := "0.0.1-SNAPSHOT"


libraryDependencies ++= Seq(
    "com.lihaoyi" %% "os-lib" % "0.7.1",
    "org.scalatest" %% "scalatest" % "3.1.2" % Test
)


// lazy val root = (project in file(".")).
//   settings(
//     inThisBuild(List(
//       organization := "ch.epfl.scala",
//       scalaVersion := "2.13.3"
//     )),
//     name := "hello-world"
//   )
