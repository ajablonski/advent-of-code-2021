package com.github.ajablonski

import com.github.ajablonski.day_12.{Connection, Path}

import scala.annotation.tailrec

object Day12 extends AocProblem[Int] {
  override def part1(filename: String): Int = {
    findPaths(parse(getRawData(filename))).size
  }

  override def part2(filename: String): Int = {
    findPaths(parse(getRawData(filename)), true).size
  }

  def findPaths(connections: Seq[Connection], canRevisitOneSmallCave: Boolean = false): Set[Path] = {
    val adjacencyMap = buildAdjacencyMap(connections)

    findPathsRecursive(Path.build("start"), adjacencyMap, canRevisitOneSmallCave)
  }

  private def findPathsRecursive(pathSoFar: Path, adjacencyMap: Map[String, Set[String]], canRevisitOneSmallCave: Boolean): Set[Path] = {
    if (pathSoFar.lastNode == "end") {
      Set(pathSoFar)
    } else {
      val nextNodes = adjacencyMap(pathSoFar.lastNode).filter(node => {
        (node.toLowerCase != node)
          || !pathSoFar.visited(node)
          || (canRevisitOneSmallCave && !pathSoFar.hasRevisitedSmallCave() && node != "start")
      })

      nextNodes
        .flatMap(node => findPathsRecursive(pathSoFar.addNode(node), adjacencyMap, canRevisitOneSmallCave))
    }
  }

  def buildAdjacencyMap(connections: Seq[Connection]): Map[String, Set[String]] = {
    connections
      .foldLeft(Map[String, Set[String]]())((mapSoFar, connection) =>
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
