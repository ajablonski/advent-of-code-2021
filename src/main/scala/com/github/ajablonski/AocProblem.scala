package com.github.ajablonski

import scala.io.Source
import scala.util.Using

abstract class AocProblem[T1, T2] {
  def main(args: Array[String]): Unit = {
    println("Part 1")
    println(profile(part1(args(0))))
    println("Part 2")
    println(profile(part2(args(0))))
  }

  def profile[T](block: => T): T = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println(s"Took ${(t1 - t0).toDouble / 1_000_000_000} seconds")
    result
  }

  def part1(filename: String): T1

  def part2(filename: String): T2

  def getRawData(filename: String): Seq[String] =
    Using(Source.fromFile(filename)) {
      reader => reader.getLines().toSeq
    }
    .getOrElse {
      println("Warning: unable to load data, returning empty sequence")
      Seq()
    }
}
