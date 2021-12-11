package com.github.ajablonski.day_11

import scala.annotation.tailrec

case class OctopusGrid(energyLevels: Map[(Int, Int), Int]) {
  private val maxRow = energyLevels.map(_._1._1).max
  private val maxCol = energyLevels.map(_._1._2).max

  def step1(): (OctopusGrid, Int) = {
    val newGrid = energyLevels
      .map {
        case ((row, col), energyLevel) => ((row, col), energyLevel + 1)
      }

    @tailrec
    def stepRecur(steppedEnergyLevels: Map[(Int, Int), Int], flashesSoFar: Set[(Int, Int)]): (Map[(Int, Int), Int], Int) = {
      val flashed = steppedEnergyLevels
        .filter(_._2 > 9)
        .keySet ++ flashesSoFar

      val coordsToUpdate = steppedEnergyLevels
        .filter(_._2 > 9)
        .toSeq
        .flatMap {
          case (coords, _) => findAdjacentCoordinates(coords)
        }
        .filterNot(flashed.contains)
      val newMap = coordsToUpdate
        .foldLeft(steppedEnergyLevels.map { case (coords, energyLevel) => (coords, if (energyLevel > 9) 0 else energyLevel) }) {
          case (newMap, coords) => newMap.updatedWith(coords)(_.map(_ + 1))
        }

      if (newMap == steppedEnergyLevels) {
        (newMap, flashed.size)
      } else {
        stepRecur(newMap, flashed)
      }
    }

    val (finalGrid, flashes) = stepRecur(newGrid, Set())
    (OctopusGrid(finalGrid), flashes)
  }


  override def toString: String = {
    energyLevels
      .groupMap(_._1._1) { case ((rowNum, colNum), energyLevel) => (colNum, energyLevel)}
      .toSeq
      .sorted
      .map { case (_, rowValues) => rowValues.toSeq.sorted.map(_._2).mkString }
      .mkString("\n")
  }

  private def findAdjacentCoordinates(point: (Int, Int)): Seq[(Int, Int)] = {
    Seq(
      (point._1 - 1, point._2 - 1),
      (point._1 - 1, point._2),
      (point._1 - 1, point._2 + 1),
      (point._1, point._2 - 1),
      (point._1, point._2 + 1),
      (point._1 + 1, point._2 - 1),
      (point._1 + 1, point._2),
      (point._1 + 1, point._2 + 1),
    ).filter(coords => (0 to maxRow).contains(coords._1) && (0 to maxCol).contains(coords._2))

  }
}


object OctopusGrid {
  def parse(lines: Seq[String]): OctopusGrid = {
    OctopusGrid(lines.zipWithIndex
      .flatMap {
        case (line, rowNum) =>
          line.zipWithIndex.map {
            case (char, colNum) =>
              (rowNum, colNum) -> char.toString.toInt
          }
      }
      .toMap)
  }

  @tailrec
  def stepN(octopusGrid: OctopusGrid, iterations: Int, flashesSoFar: Int = 0): (OctopusGrid, Int) = {
    if (iterations == 0) {
      (octopusGrid, flashesSoFar)
    } else {
      val (newGrid, flashes) = octopusGrid.step1()
      stepN(newGrid, iterations - 1, flashes + flashesSoFar)
    }
  }
}
