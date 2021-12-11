name := "advent-of-code-2021"

version := "0.1"

scalaVersion := "3.1.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

val fetch = inputKey[File]("Fetches file with particular day's data")
fetch := {
  val dayToFetch = sbt.Def.spaceDelimited("<arg>").parsed.head.toInt
  TaskUtils.fetchData(dayToFetch, baseDirectory.value)
}

TaskUtils.defineAliases()
