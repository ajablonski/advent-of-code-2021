package com.github.ajablonski

import scala.io.Source

object Day1 extends AocProblem[Int] {
  def part1(filename: String): Int = {
    countIncreases(parseInput(filename))
  }

  def part2(filename: String): Int = {
    countIncreases(parseInput(filename)
      .sliding(3)
      .map(_.sum)
      .toList)
  }

  def parseInput(filename: String): Seq[Int] = getRawData(filename).map(_.toInt)

  private def countIncreases(ints: Seq[Int]): Int = {
    ints.sliding(2)
      .count { case Seq(prev, next) => next > prev }
  }

}
