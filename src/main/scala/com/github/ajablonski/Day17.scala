package com.github.ajablonski

import com.github.ajablonski.day_17.{BoundaryArea, TrenchState}

import scala.annotation.tailrec

object Day17 extends AocProblem[Int, Int] {
  override def part1(filename: String): Int = {
    val boundaries = getParsedData(filename)
    val successfulSimulations = getPossibleInitialVelocities(boundaries)

    successfulSimulations
      .maxBy(_._2.getMaxHeight)
      ._2.getMaxHeight
  }

  override def part2(filename: String): Int = {
    val boundaries = getParsedData(filename)
    val successfulSimulations = getPossibleInitialVelocities(boundaries)

    successfulSimulations
      .size
  }

  private def getPossibleInitialVelocities(boundaries: BoundaryArea): Seq[((Int, Int), TrenchState)] = {

    val xRange = if (boundaries.xMin < 0 && boundaries.xMax > 0) {
      boundaries.xMin to boundaries.xMax
    } else if (boundaries.xMin >= 0) {
      0 to boundaries.xMax
    } else {
      boundaries.xMin to 0
    }
    xRange.flatMap { vX =>
      val extremeY = math.max(math.abs(boundaries.yMin), math.abs(boundaries.yMax))
      (-extremeY to extremeY).map { vY =>
        val initialVelocity = (vX, vY)
        (initialVelocity, simulate(TrenchState((0, 0), initialVelocity, boundaries)))
      }
    }
      .filter(_._2.hasBeenInBoundary)
  }

  @tailrec
  def simulate(trenchState: TrenchState): TrenchState = {
    if (trenchState.isPastBoundary) trenchState else simulate(trenchState.step())
  }

  def getParsedData(filename: String): BoundaryArea = {
    val line = getRawData(filename).head
    val pattern = raw"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)".r
    line match {
      case pattern(xMin, xMax, yMin, yMax) => BoundaryArea(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)
    }
  }
}



