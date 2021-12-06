package com.github.ajablonski.day_5

case class Seafloor(floor: Seq[Seq[Int]]) {
  private def addRows(rowsToAdd: Int): Seafloor = {
    Seafloor(floor :++ Seq.fill(rowsToAdd)(Seq.fill(floor.head.size)(0)))
  }

  private def addColumns(columnsToAdd: Int): Seafloor = {
    Seafloor(floor.map(row => row :++ Seq.fill(columnsToAdd)(0)))
  }

  private def rowsToAdd(line: Line): Int = {
    line match {
      case HorizontalLine(y, _, _) => Math.max(0, y - (floor.size - 1))
      case VerticalLine(_, _, maxY) => Math.max(0, maxY - (floor.size - 1))
      case DiagonalLine(_, _, _, _) => 0
    }
  }

  private def columnsToAdd(line: Line): Int = {
    line match {
      case HorizontalLine(_, _, maxX) => Math.max(0, maxX - (floor.head.size - 1))
      case VerticalLine(x, _, _) => Math.max(0, x - (floor.head.size - 1))
      case DiagonalLine(_, _, _, _) => 0
    }
  }

  def mark(line: Line): Seafloor = {
    if (rowsToAdd(line) > 0) {
      addRows(rowsToAdd(line))
        .mark(line)
    } else if (columnsToAdd(line) > 0) {
      addColumns(columnsToAdd(line))
        .mark(line)
    } else {
      val newFloor = floor.zipWithIndex
        .map { case (row, y) =>
          row
            .zipWithIndex
            .map { case (ventCount, x) =>
              line match {
                case hl: HorizontalLine => if (hl.containsPoint((x, y))) ventCount + 1 else ventCount
                case vl: VerticalLine => if (vl.containsPoint((x, y))) ventCount + 1 else ventCount
                case DiagonalLine(_, _, _, _) => ventCount
              }
            }
        }
      Seafloor(newFloor)
    }
  }

  def markWithDiagonals(line: Line): Seafloor = {
    println(s"Processing line $line")
    val newFloor = floor.zipWithIndex
      .map { case (row, y) =>
        row
          .zipWithIndex
          .map { case (ventCount, x) =>
            if (line.containsPoint((x, y))) ventCount + 1 else ventCount
          }
      }
    Seafloor(newFloor)
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


sealed trait Line {
  def containsPoint(point: (Int, Int)): Boolean
}

case class VerticalLine(x: Int, minY: Int, maxY: Int) extends Line {
  override def containsPoint(point: (Int, Int)): Boolean = {
    point match {
      case (pointX, pointY) => pointX == x && minY <= pointY && pointY <= maxY
    }
  }
}

case class HorizontalLine(y: Int, minX: Int, maxX: Int) extends Line {
  override def containsPoint(point: (Int, Int)): Boolean = {
    point match {
      case (pointX, pointY) => pointY == y && minX <= pointX && pointX <= maxX
    }
  }
}

case class DiagonalLine(xStart: Int, yStart: Int, xEnd: Int, yEnd: Int) extends Line {
  def pointsOnLine: Seq[(Int, Int)] = {
    (xStart to xEnd).zip(yStart.to(yEnd, (yEnd - yStart) / Math.abs(yEnd - yStart)))
  }

  override def containsPoint(point: (Int, Int)): Boolean = {
    point match {
      case (pointX, pointY) => pointsOnLine.contains((pointX, pointY))
    }
  }
}
