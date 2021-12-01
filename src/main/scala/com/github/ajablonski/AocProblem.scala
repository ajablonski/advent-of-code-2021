package com.github.ajablonski

import scala.io.Source

abstract class AocProblem {
  def main(args: Array[String]): Unit = {
    println("Part 1")
    println(part1(args(0)))
    println("Part 2")
    println(part2(args(0)))
  }
  
  def part1(filename: String): Int

  def part2(filename: String): Int
  
  def getRawData(filename: String): Iterator[String] = Source.fromFile(filename).getLines()
}
