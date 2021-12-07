package com.github.ajablonski.day_5

sealed trait Line {
  def containsPoint(point: (Int, Int)): Boolean
}

object Line {
  def parseLine(s: String): Line = {
    val linePattern = raw"(\d+),(\d+) -> (\d+),(\d+)".r

    s match {
      case linePattern(matches: _*) =>
        val Seq(x1, y1, x2, y2) = matches.map(_.toInt)
        if (x1 == x2) VerticalLine(x1, y1, y2)
        else if (y1 == y2) HorizontalLine(y1, x1, x2)
        else DiagonalLine(x1, y1, x2, y2)
    }
  }
}


case class VerticalLine private(x: Int, minY: Int, maxY: Int) extends Line {
  override def containsPoint(point: (Int, Int)): Boolean = {
    point match {
      case (pointX, pointY) => pointX == x && minY <= pointY && pointY <= maxY
    }
  }
}

object VerticalLine {
  def apply(x: Int, y1: Int, y2: Int): VerticalLine = {
    new VerticalLine(x, Math.min(y1, y2), Math.max(y1, y2))
  }
}


case class HorizontalLine private(y: Int, minX: Int, maxX: Int) extends Line {
  override def containsPoint(point: (Int, Int)): Boolean = {
    point match {
      case (pointX, pointY) => pointY == y && minX <= pointX && pointX <= maxX
    }
  }
}

object HorizontalLine {
  def apply(y: Int, x1: Int, x2: Int): HorizontalLine = {
    new HorizontalLine(y, Math.min(x1, x2), Math.max(x1, x2))
  }
}


case class DiagonalLine private(xStart: Int, yStart: Int, xEnd: Int, yEnd: Int) extends Line {
  override def containsPoint(point: (Int, Int)): Boolean = {
    point match {
      case (pointX, pointY) =>
        if (yEnd < yStart) {
          xStart <= pointX && pointX <= xEnd
            && yEnd <= pointY && pointY <= yStart
            && (pointY + pointX) == (xStart + yStart)
        } else {
          xStart <= pointX && pointX <= xEnd
            && yStart <= pointY && pointY <= yEnd
            && (pointY - pointX) == (yStart - xStart)
        }
    }
  }
}


object DiagonalLine {
  def apply(x1: Int, y1: Int, x2: Int, y2: Int): DiagonalLine = {
    if (x1 < x2) new DiagonalLine(x1, y1, x2, y2) else new DiagonalLine(x2, y2, x1, y1)
  }
}
