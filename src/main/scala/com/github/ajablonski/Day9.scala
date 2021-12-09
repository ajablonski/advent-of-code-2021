package com.github.ajablonski

import scala.annotation.tailrec

object Day9 extends AocProblem[Int] {
  override def part1(filename: String): Int = {
    val depthMap = parse(getRawData(filename))
    findLowPoints(depthMap)
      .map {
        case ((row, col), depth) => depth + 1
      }
      .sum
  }

  override def part2(filename: String): Int = {
    val depthMap = parse(getRawData(filename))

    val basins = findLowPoints(depthMap)
      .map(basin => {
        expandBasin(Set(basin), depthMap)
      })
    basins
      .map(_.size)
      .toSeq
      .sorted(Ordering.Int.reverse)
      .take(3)
      .product
  }

  @tailrec
  def expandBasin(basin: Set[((Int, Int), Int)], depthMap: Map[(Int, Int), Int]): Set[((Int, Int), Int)] = {
    val newEntries = basin
      .flatMap(entry => {
        findAdjacentPoints(entry._1, depthMap)
          .filter(_._2 < 9)
      }) -- basin

    if (newEntries.subsetOf(basin)) {
      basin
    } else {
      expandBasin(newEntries ++ basin, depthMap)
    }
  }

  def findAdjacentPoints(point: (Int, Int), depthMap: Map[(Int, Int), Int]): Set[((Int, Int), Int)] = {
    Set(
      (point._1 - 1, point._2),
      (point._1 + 1, point._2),
      (point._1, point._2 - 1),
      (point._1, point._2 + 1),
    ).map(p => (p, depthMap.getOrElse(p, 10)))
  }

  def parse(lines: Seq[String]): Map[(Int, Int), Int] = {
    lines.zipWithIndex
      .flatMap {
        case (row, rowIndex) =>
          row.zipWithIndex
            .map {
              case (depth, colIndex) =>
                (rowIndex, colIndex) -> depth.toString.toInt
            }
      }
      .toMap
  }

  private def findLowPoints(depthMap: Map[(Int, Int), Int]) = {
    depthMap
      .filter {
        case ((row, col), depth) =>
          depth < (
            depthMap.getOrElse((row - 1, col), 10) min
              depthMap.getOrElse((row + 1, col), 10) min
              depthMap.getOrElse((row, col - 1), 10) min
              depthMap.getOrElse((row, col + 1), 10))
      }
  }
}
