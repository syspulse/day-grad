
scalaVersion := "2.13.3"

name := "day-grad"
organization := "io.syspulse"
version := "0.0.1-SNAPSHOT"

//fork := true
connectInput in run := true

libraryDependencies ++= Seq(
    "com.lihaoyi" %% "os-lib" % "0.7.1",
    "org.scalatest" %% "scalatest" % "3.1.2" % Test
)
