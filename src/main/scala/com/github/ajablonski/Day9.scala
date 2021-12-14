package com.github.ajablonski

import scala.annotation.tailrec

object Day9 extends AocProblem[Int, Int] {
  private val MaxDepth = 9

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

    findLowPoints(depthMap)
      .map(basin => {
        expandBasin(Set(basin), depthMap).size
      })
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
          .filter(_._2 < MaxDepth)
      })

    if (newEntries.subsetOf(basin)) {
      basin
    } else {
      expandBasin(newEntries ++ basin, depthMap)
    }
  }

  def findAdjacentPoints(point: (Int, Int), depthMap: Map[(Int, Int), Int]): Set[((Int, Int), Int)] = {
    val (row, col) = point
    Set(
      (row - 1, col),
      (row + 1, col),
      (row, col - 1),
      (row, col + 1),
    ).map(p => (p, depthMap.getOrElse(p, MaxDepth)))
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

  private def findLowPoints(depthMap: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    depthMap
      .filter {
        case (point, depth) =>
          depth < findAdjacentPoints(point, depthMap).map(_._2).min
      }
  }
}
