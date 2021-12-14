package com.github.ajablonski

object Day7 extends AocProblem[Int, Int] {
  val part1CostFunction: (Int, Int) => Int = (index: Int, finalIndex: Int) => {
    Math.abs(finalIndex - index)
  }
  
  val part2CostFunction: (Int, Int) => Int = (index: Int, finalIndex: Int) => {
    val n = Math.abs(finalIndex - index)
    n * (n + 1) / 2
  }
  
  override def part1(filename: String): Int = {
    minimizeCost(filename, part1CostFunction)
  }

  override def part2(filename: String): Int = {
    minimizeCost(filename, part2CostFunction)
  }

  private def minimizeCost(filename: String, costFunction: (Int, Int) => Int): Int = {
    val positions = getRawData(filename).head.split(",").map(_.toInt)
    val positionsMap = buildPositionsMap(positions)

    (positions.min to positions.max)
      .map(finalPosition => calculateTotalCost(costFunction)(positionsMap, finalPosition))
      .min
  }

  def buildPositionsMap(initialPositions: Seq[Int]): Map[Int, Int] = {
    initialPositions
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }

  def calculateTotalCost(costingFunction: (Int, Int) => Int)(positionMap: Map[Int, Int], finalPosition: Int): Int = {
    positionMap
      .map { case (index, count) => count * costingFunction(index, finalPosition) }
      .sum
  }
}
