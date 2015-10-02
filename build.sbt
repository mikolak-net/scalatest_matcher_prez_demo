import sbt._
import Keys._

name := "scalatest_prez_demo"
organization := "com.softwaremill"
version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= { 
  val scalaTestVersion = "2.2.5"
  Seq(
    "org.scalactic"                                 %% "scalactic" % scalaTestVersion,
    "org.scalatest"                                 %% "scalatest" % scalaTestVersion % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.1"
  )}

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-reflect" % "2.11.6",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3")


scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-Ywarn-dead-code",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding", "UTF-8"
)

scalacOptions in Test ++= Seq("-Yrangepos")

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

resolvers ++= Seq("sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "sonatype-release"  at "http://oss.sonatype.org/content/repositories/releases",
	          "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
		  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

//mainClass in assembly := Some("net.mikolak.main")

//fork in Test := true
