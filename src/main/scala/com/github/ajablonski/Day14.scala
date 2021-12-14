package com.github.ajablonski

import scala.annotation.tailrec

object Day14 extends AocProblem[BigInt, BigInt] {
  override def part1(filename: String): BigInt = {
    val ((seed, lastChar), rules) = parse(getRawData(filename))

    val finalValue = stepN(seed, rules, 10)
    val letterCounts = countLetters(finalValue, lastChar)

    letterCounts.values.max - letterCounts.values.min
  }

  override def part2(filename: String): BigInt = {
    val ((seed, lastChar), rules) = parse(getRawData(filename))

    val finalValue = stepN(seed, rules, 40)
    val letterCounts = countLetters(finalValue, lastChar)

    letterCounts.values.max - letterCounts.values.min
  }

  def parse(lines: Seq[String]): ((Map[String, BigInt], Char), Map[String, String]) = {
    val ruleRegex = raw"(.{2}) -> (.)".r
    val (seedPart, rulesPart) = lines.splitAt(lines.indexWhere(_.isBlank))

    (
      (
        seedPart.head
          .zip(seedPart.head.tail)
          .map { case (char1, char2) => s"$char1$char2" }
          .foldLeft(Map[String, BigInt]())((mapSoFar, newEntry) => mapSoFar.updatedWith(newEntry)(_.map(_ + 1).orElse(Some(BigInt(1)))))
        ,
        seedPart.head.last
      ),
      rulesPart.tail.map {
        case ruleRegex(matchLetters, letterToInsert) => matchLetters -> letterToInsert
      }.toMap)
  }

  def step(seed: Map[String, BigInt], rules: Map[String, String]): Map[String, BigInt] = {
    seed
      .foldLeft(Map[String, BigInt]())((mapSoFar, currentEntry) => currentEntry match {
        case (currentKey, currentCount) =>
          val letterToInsert = rules(currentKey)
          val newEntry1 = s"${currentKey.head}$letterToInsert"
          val newEntry2 = s"$letterToInsert${currentKey.tail}"
          mapSoFar
            .updatedWith(newEntry1)(_.map(_ + currentCount).orElse(Some(currentCount)))
            .updatedWith(newEntry2)(_.map(_ + currentCount).orElse(Some(currentCount)))
            .filter(_._2 > 0)
      })
  }

  @tailrec
  def stepN(seed: Map[String, BigInt], rules: Map[String, String], times: Int): Map[String, BigInt] = {
    if (times == 0) {
      seed
    } else {
      stepN(step(seed, rules), rules, times - 1)
    }
  }

  def countLetters(seed: Map[String, BigInt], lastChar: Char): Map[Char, BigInt] = {
    seed
      .foldLeft(Map[Char, BigInt]())((mapSoFar, entry) => {
        entry match {
          case (letterPair, count) => mapSoFar
            .updatedWith(letterPair.charAt(0))(_.map(_ + count).orElse(Some(count)))
        }
      })
      .updatedWith(lastChar)(_.map(_ + 1).orElse(Some(BigInt(1))))
  }
}
