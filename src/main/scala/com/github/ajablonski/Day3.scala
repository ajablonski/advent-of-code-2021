package com.github.ajablonski

import scala.annotation.tailrec

object Day3 extends AocProblem[Int] {
  override def part1(filename: String): Int = {
    val inputData = getRawData(filename)
    gamma(inputData) * epsilon(inputData)
  }

  override def part2(filename: String): Int = {
    val inputData = getRawData(filename)

    oxygenGeneratorRating(inputData) * co2ScrubberRating(inputData)
  }

  def gamma(lines: Seq[String]): Int =
    Integer.parseInt(getBitCountsByIndex(lines).map(selectByMostCommonBit).mkString, 2)

  def epsilon(lines: Seq[String]): Int =
    Integer.parseInt(getBitCountsByIndex(lines).map(selectByLeastCommonBit).mkString, 2)

  def oxygenGeneratorRating: Seq[String] => Int =
    filterNumbersBasedOnBitCountCriteria(selectByMostCommonBit)(0)

  def co2ScrubberRating: Seq[String] => Int =
    filterNumbersBasedOnBitCountCriteria(selectByLeastCommonBit)(0)


  def getBitCountsByIndex(input: Seq[String]): Seq[Map[Char, Int]] = {
    input.flatMap(line => line.zipWithIndex)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .groupMapReduce({ case ((char, index), count) => index })({ case ((char, index), count) => Map(char -> count) })({ case (mapA, mapB) => mapA ++ mapB })
      .toSeq
      .sortBy { case (index, _) => index }
      .map { case (_, bitCountMap) => bitCountMap }
  }

  private val selectByMostCommonBit: Map[Char, Int] => Char =
    bitCounts => bitCounts.maxBy { case (char, count) => (count, char) }._1
  private val selectByLeastCommonBit: Map[Char, Int] => Char =
    bitCounts => bitCounts.minBy { case (char, count) => (count, char) }._1

  @tailrec
  private def filterNumbersBasedOnBitCountCriteria(bitSelectionCriteria: Map[Char, Int] => Char)
                                                  (bitIndex: Int)
                                                  (input: Seq[String]): Int = {
    if (input.length == 1) {
      Integer.parseInt(input.head, 2)
    } else {
      val bitCounts = input.groupMapReduce(line => line(bitIndex))(_ => 1)(_ + _)
      val bitToSelect = bitSelectionCriteria(bitCounts)
      filterNumbersBasedOnBitCountCriteria(bitSelectionCriteria)(bitIndex + 1)(input.filter(line => line(bitIndex) == bitToSelect))
    }
  }
}
