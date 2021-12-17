package com.github.ajablonski

import com.github.ajablonski.util.NumberGrid

import scala.annotation.tailrec
import scala.util.Try

object Day15 extends AocProblem[Int, Int] {

  import NumberGrid.findAdjacentPoints

  private val start = (0, 0)

  override def part1(filename: String): Int = {
    findLeastRiskPath(NumberGrid.parse(getRawData(filename)))
      .tail
      .map(_._2)
      .sum
  }

  override def part2(filename: String): Int = {
    findLeastRiskPath(NumberGrid.parse(expandMap(getRawData(filename))))
      .tail
      .map(_._2)
      .sum

  }

  def findLeastRiskPath(riskMap: Map[(Int, Int), Int]): Seq[((Int, Int), Int)] = {
    dijkstraShortedPath(riskMap,
      riskMap.map((k, v) => k -> (if (k == start) 0 else Int.MaxValue)),
      riskMap.map((k, v) => k -> None),
      riskMap.keySet,
      riskMap.keySet.max)
  }

  def expandMap(currentMap: Seq[String]): Seq[String] = {
    (0 to 4).flatMap(rowNum =>
      currentMap.map {
        row =>
          (0 to 4).map(colNum => row.map(character => {
            val newInt = character.toString.toInt + colNum + rowNum
            if (newInt >= 10) {
              newInt - 9
            } else {
              newInt
            }
          }).mkString).mkString
      }
    )
  }

  @tailrec
  private def dijkstraShortedPath(riskMap: Map[(Int, Int), Int],
                                  distanceMap: Map[(Int, Int), Int],
                                  previousNodeMap: Map[(Int, Int), Option[(Int, Int)]],
                                  unvisited: Set[(Int, Int)],
                                  destination: (Int, Int))
  : Seq[((Int, Int), Int)] = {
    val closestPoint = distanceMap.filter((k, v) => unvisited.contains(k)).minBy(_._2)._1
    if (unvisited.isEmpty || closestPoint == destination) {
      tracePathFromEndToStart(riskMap, previousNodeMap, Seq(destination -> riskMap(destination)))
    } else {
      val adjacentCellsToCheck = riskMap
        .findAdjacentPoints(closestPoint)
        .map(_._1)
        .intersect(unvisited)

      val (newDistanceMap, newPreviousNodeMap) = adjacentCellsToCheck
        .foldLeft((distanceMap, previousNodeMap)) {
          case ((distanceMap, previousNodeMap), cell) =>
            val alt = distanceMap(closestPoint) + riskMap(cell)
            if (alt < distanceMap(cell)) {
              (distanceMap.updated(cell, alt),
                previousNodeMap.updated(cell, Some(closestPoint)))
            } else {
              (distanceMap, previousNodeMap)
            }
        }
      dijkstraShortedPath(riskMap, newDistanceMap, newPreviousNodeMap, unvisited - closestPoint, destination)
    }
  }

  @tailrec
  private def tracePathFromEndToStart(riskMap: Map[(Int, Int), Int],
                                      previousNodeMap: Map[(Int, Int), Option[(Int, Int)]],
                                      pathSoFar: Seq[((Int, Int), Int)])
  : Seq[((Int, Int), Int)] = {
    if (pathSoFar.head._1 == start) {
      pathSoFar
    } else {
      val previousNode = previousNodeMap(pathSoFar.head._1).get
      tracePathFromEndToStart(riskMap, previousNodeMap, (previousNode -> riskMap(previousNode)) +: pathSoFar)
    }
  }
}
