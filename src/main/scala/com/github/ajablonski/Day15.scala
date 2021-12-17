package com.github.ajablonski

import com.github.ajablonski.util.NumberGrid

import scala.annotation.tailrec
import scala.collection.mutable
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
    dijkstraShortestPath(riskMap,
      riskMap.map((k, v) => k -> (if (k == start) 0 else Int.MaxValue)),
      riskMap.map((k, v) => k -> None),
      mutable.PriorityQueue[(Int, (Int, Int))]((0, start))((a, b) => b._1.compareTo(a._1)),
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
  private def dijkstraShortestPath(riskMap: Map[(Int, Int), Int],
                                  distanceMap: Map[(Int, Int), Int],
                                  previousNodeMap: Map[(Int, Int), Option[(Int, Int)]],
                                  unvisited: mutable.PriorityQueue[(Int, (Int, Int))],
                                  destination: (Int, Int))
  : Seq[((Int, Int), Int)] = {
    if (unvisited.isEmpty || unvisited.headOption.map(_._2).contains(destination)) {
      tracePathFromEndToStart(riskMap, previousNodeMap, Seq(destination -> riskMap(destination)))
    } else {
      val closestPoint = unvisited.dequeue()._2

      val adjacentCellsToCheck = riskMap
        .findAdjacentPoints(closestPoint)
        .map(_._1)

      val (newDistanceMap, newPreviousNodeMap, newUnvisited) = adjacentCellsToCheck
        .foldLeft((distanceMap, previousNodeMap, unvisited)) {
          case ((distanceMap, previousNodeMap, unvisited), cell) =>
            val alt = distanceMap(closestPoint) + riskMap(cell)
            if (alt < distanceMap(cell)) {
              (distanceMap.updated(cell, alt),
                previousNodeMap.updated(cell, Some(closestPoint)),
                if (!unvisited.exists(_._2 == cell)) unvisited.addOne((alt, cell)) else unvisited)
            } else {
              (distanceMap, previousNodeMap, unvisited)
            }
        }
      dijkstraShortestPath(riskMap, newDistanceMap, newPreviousNodeMap, newUnvisited, destination)
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
