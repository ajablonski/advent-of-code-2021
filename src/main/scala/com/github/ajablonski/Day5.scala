package com.github.ajablonski

import com.github.ajablonski.day_5.{DiagonalLine, HorizontalLine, Line, Seafloor, VerticalLine}

object Day5 extends AocProblem {
  override def part1(filename: String): Int = {
    val lines = parse(getRawData(filename))
    lines
      .foldLeft(Seafloor.initializeSeafloorFromLines(lines))((floor, line) => floor.mark(line))
      .floor
      .map(_.count(_ > 1))
      .sum
  }

  override def part2(filename: String): Int = {
    val lines = parse(getRawData(filename))
    lines
      .foldLeft(Seafloor.initializeSeafloorFromLines(lines))((floor, line) => floor.markWithDiagonals(line))
      .floor
      .map(_.count(_ > 1))
      .sum
  }

  private val linePattern = raw"(\d+),(\d+) -> (\d+),(\d+)".r

  def parse(lines: Seq[String]): Seq[Line] = {
    lines.map {
      case linePattern(x1, y1, x2, y2) =>
        val x1Int = x1.toInt
        val x2Int = x2.toInt
        val y1Int = y1.toInt
        val y2Int = y2.toInt
        if (x1Int == x2Int) VerticalLine(x1Int, Math.min(y1Int, y2Int), Math.max(y1Int, y2Int))
        else if (y1Int == y2Int) HorizontalLine(y1Int, Math.min(x1Int, x2Int), Math.max(x1Int, x2Int))
        else if (x1Int < x2Int) DiagonalLine(x1Int, y1Int, x2Int, y2Int)
        else DiagonalLine(x2Int, y2Int, x1Int, y1Int)
    }
  }
}
