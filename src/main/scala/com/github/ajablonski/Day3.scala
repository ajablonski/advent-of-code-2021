package com.github.ajablonski

import scala.annotation.tailrec

object Day3 extends AocProblem {
  override def part1(filename: String): Int = {
    val inputData = getRawData(filename)
    gamma(inputData) * epsilon(inputData)
  }

  override def part2(filename: String): Int = {
    val inputData = getRawData(filename)

    oxygenGeneratorRating(inputData) * co2ScrubberRating(inputData)
  }

  def gamma(lines: Seq[String]): Int = {
    Integer.parseInt(findMostCommonBits(lines).mkString, 2)
  }

  private def findMostCommonBits(lines: Seq[String]): Seq[String] =
    parse(lines).map(bitCountMap => bitCountMap.maxBy(_._2)._1)

  def epsilon(lines: Seq[String]): Int = {
    Integer.parseInt(findLeastCommonBits(lines).mkString, 2)
  }

  private def findLeastCommonBits(lines: Seq[String]): Seq[String] = {
    parse(lines).map(bitCountMap => bitCountMap.minBy(_._2)._1)
  }

  @tailrec
  def oxygenGeneratorRating(input: Seq[String], bitIndex: Int = 0): Int = {
    if (input.length == 1) {
      Integer.parseInt(input.head, 2)
    } else {
      val bitCounts = input.map(_(bitIndex)).groupMapReduce(identity)(_ => 1)(_ + _)
      val mostCommonBit = if (bitCounts.size == 1) {
        bitCounts.head._1
      } else if (bitCounts('0') > bitCounts('1')) {
        '0'
      } else {
        '1'
      }
      oxygenGeneratorRating(input.filter(_.apply(bitIndex) == mostCommonBit), bitIndex + 1)
    }
  }

  @tailrec
  def co2ScrubberRating(input: Seq[String], bitIndex: Int = 0): Int = {
    if (input.length == 1) {
      Integer.parseInt(input.head, 2)
    } else {
      val bitCounts = input.map(_(bitIndex)).groupMapReduce(identity)(_ => 1)(_ + _)
      val leastCommonBit = if (bitCounts.size == 1) {
        bitCounts.head._1
      } else if (bitCounts('0') <= bitCounts('1')) {
        '0'
      } else {
        '1'
      }
      co2ScrubberRating(input.filter(_.apply(bitIndex) == leastCommonBit), bitIndex + 1)
    }
  }

  def parse(input: Seq[String]): Seq[Map[String, Int]] = {
    val flatMapped = input.flatMap(line => line.zipWithIndex)
    val grouped = flatMapped
      .groupBy { case (char, index) => index }
    val mapped = grouped
      .map { case (bitIndex, values) =>
        (bitIndex,
          values
            .groupBy(_._1)
            .map { case (bitValue, entries) => (bitValue.toString, entries.length) })
      }
    val sorted = mapped
      .toSeq
      .sortBy(_._1)
    val lastMapped = sorted
      .map(_._2)
    lastMapped
  }
}
