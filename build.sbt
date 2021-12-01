import sbt.Def.spaceDelimited

import java.net.HttpURLConnection
import java.nio.file.{Files, StandardCopyOption}
import scala.io.Source

name := "aoc-2021"

version := "0.1"

scalaVersion := "3.1.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

val fetch = inputKey[File]("Fetches file with particular day's data")
fetch := {
  val dayToFetch = spaceDelimited("<arg>").parsed.head.toInt
  val cookieFile = Source.fromFile(baseDirectory.value / "data" / "sessioncookie.txt")
  val cookie = cookieFile.getLines().next()
  cookieFile.close()
  val connection = new URL(s"https://adventofcode.com/2021/day/$dayToFetch/input")
    .openConnection()
    .asInstanceOf[HttpURLConnection]
  connection.setRequestProperty("Cookie", cookie)
  connection.setRequestMethod("GET")

  val outputFile  = baseDirectory.value / "data" / s"day$dayToFetch.txt"

  Files.copy(connection.getInputStream, outputFile.toPath, StandardCopyOption.REPLACE_EXISTING)

  outputFile
}