package com.github.ajablonski

import scala.io.Source

object Day1 extends AocProblem {
  def part1(filename: String): Int = {
    countIncreases(parseInput(filename))
  }

  def part2(filename: String): Int = {
    val lines = parseInput(filename)

    countIncreases(
      lines.drop(2)
        .zip(lines.drop(1))
        .map(_ + _)
        .zip(lines)
        .map(_ + _))
  }

  def parseInput(filename: String): List[Int] = getRawData(filename).map(_.toInt).toList
  
  private def countIncreases(ints: List[Int]): Int = {
    ints.sliding(2)
      .count { case List(next, prev) => next > prev }
  }

}
