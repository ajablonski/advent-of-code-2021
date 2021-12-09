package com.github.ajablonski

import com.github.ajablonski.day_8.DigitFinders

object Day8 extends AocProblem[Int] {
  override def part1(filename: String): Int = {
    getRawData(filename)
      .flatMap(line => decodeLine(line).split(""))
      .count(digit => Set("1", "4", "7", "8").contains(digit))
  }

  override def part2(filename: String): Int = {
    getRawData(filename)
      .map(line => decodeLine(line).toInt)
      .sum
  }

  def decodeLine(line: String): String = {
    val (allSegmentCombinations, outputSegmentCombinations) = parseLine(line)
    translateOutput(outputSegmentCombinations, DigitFinders.findAll(allSegmentCombinations, Map()))
  }

  def parseLine(line: String): (Set[Set[String]], Seq[Set[String]]) = {
    val Array(allDigitCombinations: String, outputCombinations: String) = line.split(" \\| ")
    (parseDigitCombinations(allDigitCombinations).toSet, parseDigitCombinations(outputCombinations))
  }

  def translateOutput(outputSegmentSets: Seq[Set[String]], digitMap: Map[Int, Set[String]]): String = {
    val reversedMap = digitMap
      .map(entry => entry._2 -> entry._1)
    outputSegmentSets
      .map(outputSegmentSet => reversedMap.get(outputSegmentSet).map(_.toString).getOrElse("?"))
      .mkString
  }

  private def parseDigitCombinations(digitCombinationString: String): Seq[Set[String]] = {
    digitCombinationString.split(" ")
      .map(_.split("").toSet)
  }
}
