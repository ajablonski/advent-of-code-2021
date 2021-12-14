package com.github.ajablonski

import com.github.ajablonski.day_5.{DiagonalLine, HorizontalLine, Line, Seafloor, VerticalLine}

object Day5 extends AocProblem[Int, Int] {
  override def part1(filename: String): Int = {
    findSeafloorSpotsWithMultipleVents(filename, (floor, line) => floor.mark(line))
  }

  override def part2(filename: String): Int = {
    findSeafloorSpotsWithMultipleVents(filename, (floor, line) => floor.markWithDiagonals(line))
  }

  def parse(lines: Seq[String]): Seq[Line] = {
    lines.map { Line.parseLine }
  }

  private def findSeafloorSpotsWithMultipleVents(filename: String, markFunction: (Seafloor, Line) => Seafloor): Int = {
    val lines = parse(getRawData(filename))
    lines
      .foldLeft(Seafloor.initializeSeafloorFromLines(lines))(markFunction)
      .floor
      .map(_.count(_ > 1))
      .sum
  }

}
