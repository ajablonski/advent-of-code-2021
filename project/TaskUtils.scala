import sbt.Keys.{onLoad, onUnload}
import sbt.{BasicCommands, GlobalScope, Setting, SettingKey, State}

import java.io.File
import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, StandardCopyOption}
import scala.io.Source
import scala.reflect.io.Path.jfile2path

object TaskUtils {
  def fetchData(day: Int, baseDirectory: File): File = {
    val cookieFile = Source.fromFile((baseDirectory / "data" / "sessioncookie.txt").jfile)
    val cookie = cookieFile.getLines().next()
    cookieFile.close()
    val connection = new URL(s"https://adventofcode.com/2021/day/$day/input")
      .openConnection()
      .asInstanceOf[HttpURLConnection]
    connection.setRequestProperty("Cookie", cookie)
    connection.setRequestMethod("GET")

    val outputFile = baseDirectory / "data" / s"day$day.txt"

    Files.copy(connection.getInputStream, outputFile.jfile.toPath, StandardCopyOption.REPLACE_EXISTING)

    outputFile.jfile
  }

  def defineAliases(): Seq[Setting[State => State]] = {
    def compose(setting: SettingKey[State => State], f: State => State) =
      (GlobalScope / setting) ~= (_ compose f)

    val add = (s: State) => {
      val stateWithDays = (1 to 25)
        .foldLeft(s) { (state, day) =>
          BasicCommands.addAlias(state, s"run$day", s"runMain com.github.ajablonski.Day$day data/day$day.txt")
        }

      BasicCommands.addAlias(stateWithDays, "runAll", (1 to 25).map(d => s"run$d").mkString("; "))
    }
    val remove = (s: State) => {
      val stateWithoutDays = (1 to 25)
        .foldLeft(s) { (state, day) =>
          BasicCommands.removeAlias(state, s"run$day")
        }

      BasicCommands.removeAlias(stateWithoutDays, "runAll")
    }
    Seq(compose(onLoad, add), compose(onUnload, remove))
  }
}
