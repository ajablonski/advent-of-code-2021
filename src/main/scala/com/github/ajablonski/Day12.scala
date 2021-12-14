package com.github.ajablonski

import com.github.ajablonski.day_12.{Connection, Path, Cave}

import scala.annotation.tailrec

object Day12 extends AocProblem[Int, Int] {
  override def part1(filename: String): Int = {
    findPaths(parse(getRawData(filename))).size
  }

  override def part2(filename: String): Int = {
    findPaths(parse(getRawData(filename)), true).size
  }

  def findPaths(connections: Seq[Connection], canRevisitOneSmallCave: Boolean = false): Set[Path] = {
    val adjacencyMap = buildAdjacencyMap(connections)

    buildPaths(Path.build("start"), adjacencyMap, canRevisitOneSmallCave)
  }

  private def buildPaths(pathSoFar: Path, adjacencyMap: Map[Cave, Set[Cave]], canRevisitOneSmallCave: Boolean): Set[Path] = {
    if (pathSoFar.lastNode == Cave("end")) {
      Set(pathSoFar)
    } else {
      adjacencyMap(pathSoFar.lastNode)
        .filter(cave => {
          (!cave.isSmall)
            || !pathSoFar.visited(cave)
            || (canRevisitOneSmallCave && !pathSoFar.hasRevisitedSmallCave && cave != Cave("start"))
        })
        .flatMap(node => buildPaths(pathSoFar.addNode(node), adjacencyMap, canRevisitOneSmallCave))
    }
  }

  def buildAdjacencyMap(connections: Seq[Connection]): Map[Cave, Set[Cave]] = {
    connections
      .foldLeft(Map[Cave, Set[Cave]]())((mapSoFar, connection) =>
        mapSoFar
          .updatedWith(connection.start)(_.map(_ + connection.end).orElse(Some(Set(connection.end))))
          .updatedWith(connection.end)(_.map(_ + connection.start).orElse(Some(Set(connection.start))))
      )
  }

  def parse(lines: Seq[String]): Seq[Connection] = {
    lines.map(
      line => {
        val Array(start: String, end: String) = line.split("-")
        Connection(start, end)
      }
    )
  }
}
