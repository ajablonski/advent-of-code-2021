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
}
