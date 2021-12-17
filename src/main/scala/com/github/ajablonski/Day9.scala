package com.github.ajablonski

import scala.annotation.tailrec

import com.github.ajablonski.util.NumberGrid

object Day9 extends AocProblem[Int, Int] {

  import NumberGrid.findAdjacentPoints

  private val MaxDepth = 9

  override def part1(filename: String): Int = {
    val depthMap = NumberGrid.parse(getRawData(filename))
    findLowPoints(depthMap)
      .map {
        case ((row, col), depth) => depth + 1
      }
      .sum
  }

  override def part2(filename: String): Int = {
    val depthMap = NumberGrid.parse(getRawData(filename))

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
        depthMap.findAdjacentPoints(entry._1, Some(MaxDepth))
          .filter(_._2 < MaxDepth)
      })

    if (newEntries.subsetOf(basin)) {
      basin
    } else {
      expandBasin(newEntries ++ basin, depthMap)
    }
  }

  private def findLowPoints(depthMap: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    depthMap
      .filter {
        case (point, depth) =>
          depth < depthMap.findAdjacentPoints(point, Some(MaxDepth)).map(_._2).min
      }
  }
}
