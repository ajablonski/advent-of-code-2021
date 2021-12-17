package com.github.ajablonski.util

object NumberGrid {
  extension (m: Map[(Int, Int), Int])  {
    def findAdjacentPoints(point: (Int, Int), defaultValue: Option[Int] = None): Set[((Int, Int), Int)] = {
      val (row, col) = point
      Set(
        (row - 1, col),
        (row + 1, col),
        (row, col - 1),
        (row, col + 1),
      ).flatMap(point => m.get(point).orElse(defaultValue).map((point, _)))
    }
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
}
