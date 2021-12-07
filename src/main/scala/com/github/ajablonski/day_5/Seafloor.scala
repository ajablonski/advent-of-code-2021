package com.github.ajablonski.day_5

case class Seafloor(floor: Seq[Seq[Int]]) {

  private def addRows(rowsToAdd: Int): Seq[Seq[Int]] = {
    floor :++ Seq.fill(rowsToAdd)(Seq.fill(floor.head.size)(0))
  }

  private def addColumns(columnsToAdd: Int): Seq[Seq[Int]] = {
    floor.map(row => row :++ Seq.fill(columnsToAdd)(0))
  }

  private def rowsToAdd(line: Line): Int = {
    line match {
      case HorizontalLine(y, _, _) => Math.max(0, y - (floor.size - 1))
      case VerticalLine(_, _, maxY) => Math.max(0, maxY - (floor.size - 1))
      case DiagonalLine(_, y1, _, y2) => (y1 - (floor.size - 1)) max (y2 - (floor.size - 1)) max 0
    }
  }

  private def columnsToAdd(line: Line): Int = {
    line match {
      case HorizontalLine(_, _, maxX) => Math.max(0, maxX - (floor.head.size - 1))
      case VerticalLine(x, _, _) => Math.max(0, x - (floor.head.size - 1))
      case DiagonalLine(x1, _, x2, _) => (x1 - (floor.head.size - 1)) max (x2 - (floor.head.size - 1)) max 0
    }
  }

  def resizeFloor(line: Line): Seq[Seq[Int]] = {
    val rowsToAddCount = rowsToAdd(line)
    val columnsToAddCount = columnsToAdd(line)

    val resizedWithRows = if (rowsToAddCount > 0) addRows(rowsToAddCount) else floor
    val resizedWithCols = if (columnsToAddCount > 0) addColumns(columnsToAddCount) else resizedWithRows

    resizedWithCols
  }

  def mark(line: Line, handleDiagonals: Boolean = false): Seafloor = {
    val newFloor = resizeFloor(line)
      .zipWithIndex
      .map { case (row, y) =>
        row
          .zipWithIndex
          .map { case (ventCount, x) =>
            line match {
              case l: (HorizontalLine | VerticalLine) => if (l.containsPoint((x, y))) ventCount + 1 else ventCount
              case l: DiagonalLine if handleDiagonals => if (l.containsPoint((x, y))) ventCount + 1 else ventCount
              case _ => ventCount
            }
          }
      }
    Seafloor(newFloor)
  }

  def markWithDiagonals(line: Line): Seafloor = {
    mark(line, true)
  }
}

object Seafloor {
  def initializeSeafloorFromLines(lines: Seq[Line]): Seafloor = {
    val (maxX, maxY) = lines
      .foldLeft((0, 0))((maxCoords, line) => {
        (maxCoords, line) match {
          case ((largestX, largestY), VerticalLine(x, minY, maxY)) => (largestX max x, largestY max maxY)
          case ((largestX, largestY), HorizontalLine(y, minX, maxX)) => (largestX max maxX, largestY max y)
          case ((largestX, largestY), DiagonalLine(x1, y1, x2, y2)) => (largestX max x1 max x2, largestY max y1 max y2)
        }
      })
    Seafloor(
      Seq.fill(maxY + 1)(
        Seq.fill(maxX + 1)(0)
      )
    )
  }
}
