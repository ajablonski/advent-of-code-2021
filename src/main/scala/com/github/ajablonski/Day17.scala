package com.github.ajablonski

import com.github.ajablonski.day_17.{BoundaryArea, TrenchState}

import scala.annotation.tailrec
import scala.math.{min, max, abs, sqrt}

object Day17 extends AocProblem[Int, Int] {
  override def part1(filename: String): Int = {
    simulateSuccessfulInitialVelocities(getParsedData(filename))
      .maxBy(_.getMaxHeight)
      .getMaxHeight
  }

  override def part2(filename: String): Int = {
    simulateSuccessfulInitialVelocities(getParsedData(filename))
      .size
  }

  private def simulateSuccessfulInitialVelocities(boundaries: BoundaryArea): Seq[TrenchState] = {
    val minAbsoluteXVelocity = ((1 + sqrt(1 + 8 * boundaries.xMin)) / 2).toInt
    val xRange = (min(boundaries.xMin, minAbsoluteXVelocity)
      to max(-minAbsoluteXVelocity, boundaries.xMax))
    val minAbsoluteYVelocity = ((1 + sqrt(1 + 8 * boundaries.yMin)) / 2).toInt
    val yRange = (min(boundaries.yMin, minAbsoluteYVelocity)
      to max(abs(boundaries.yMin), abs(boundaries.yMax)))
    for (vX <- xRange;
         vY <- yRange;
         finalState = simulate(TrenchState((0, 0), (vX, vY), boundaries)) if finalState.hasBeenInBoundary)
    yield finalState
  }

  @tailrec
  def simulate(trenchState: TrenchState): TrenchState = {
    if (trenchState.isInBoundary || trenchState.isPastBoundary) trenchState else simulate(trenchState.step())
  }

  def getParsedData(filename: String): BoundaryArea = {
    val line = getRawData(filename).head
    val pattern = raw"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)".r
    line match {
      case pattern(xMin, xMax, yMin, yMax) => BoundaryArea(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)
    }
  }
}



