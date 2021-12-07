package com.github.ajablonski

object Day7 extends AocProblem[Int] {
  override def part1(filename: String): Int = {
    val positions = getRawData(filename).head.split(",").map(_.toInt).sorted
    val median = if (positions.length % 2 == 0) {
      (positions(positions.length / 2) + positions(positions.length / 2 - 1)) / 2
    } else {
      positions(positions.length / 2)
    }

    positions
      .map(position => Math.abs(position - median))
      .sum
  }

  override def part2(filename: String): Int = {
    val positions = getRawData(filename).head.split(",").map(_.toInt)
    val positionsMap = buildPositionList(positions)

    (positions.min to positions.max)
      .map(finalPosition => calculateTotalCost(positionsMap, finalPosition))
      .min
  }

  def buildPositionList(initialPositions: Seq[Int]): Map[Int, Int] = {
    initialPositions
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }

  def calculateTotalCost(positionMap: Map[Int, Int], finalPosition: Int): Int = {
    positionMap
      .map {
        case (index, count) =>
          val n = Math.abs(finalPosition - index)
          count * n * (n + 1) / 2
      }
      .sum
  }
}
