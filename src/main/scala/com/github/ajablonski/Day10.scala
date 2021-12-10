package com.github.ajablonski

import scala.annotation.tailrec

object Day10 extends AocProblem[Long] {
  override def part1(filename: String): Long = {
    getRawData(filename)
      .flatMap(findUnexpectedClosings)
      .map(scoreCorruption)
      .sum
  }

  override def part2(filename: String): Long = {
    val scores = getRawData(filename)
      .filter(findUnexpectedClosings(_).isEmpty)
      .map(line => scoreCompletion(complete(line).split("").toList.asInstanceOf[List[ClosingChar]]))
      .sorted

    scores(scores.size / 2)
  }

  def findUnexpectedClosings(line: String): Option[ExpectedButWas] = {
    val bracketRegex = raw".*\[([}>)]).*".r
    val braceRegex = raw".*\{([]>)]).*".r
    val parenRegex = raw".*\(([]}>]).*".r
    val angleRegex = raw".*<([]})]).*".r

    removeMatchedChunks(line) match {
      case bracketRegex(butWas) => Some(ExpectedButWas("]", butWas.asInstanceOf[ClosingChar]))
      case braceRegex(butWas) => Some(ExpectedButWas("}", butWas.asInstanceOf[ClosingChar]))
      case parenRegex(butWas) => Some(ExpectedButWas(")", butWas.asInstanceOf[ClosingChar]))
      case angleRegex(butWas) => Some(ExpectedButWas(">", butWas.asInstanceOf[ClosingChar]))
      case _ => None
    }
  }

  def complete(line: String): String = {
    removeMatchedChunks(line)
      .map(completionMap)
      .reverse
      .mkString
  }

  def scoreCorruption(expectedButWas: ExpectedButWas): Int = {
    corruptionScoreMap(expectedButWas.was)
  }
  
  @tailrec
  def scoreCompletion(remainingSymbols: List[ClosingChar], scoreSoFar: Long = 0): Long = {
    remainingSymbols match {
      case Nil => scoreSoFar
      case (head: ClosingChar) :: rest => scoreCompletion(rest, 5 * scoreSoFar + completionScoreMap(head))
    }
  }

  @tailrec
  def removeMatchedChunks(line: String): String = {
    val innerChunkRegex = raw"(.*)(\{}|\[]|\(\)|<>)(.*)".r
    val removed = line match {
      case innerChunkRegex(leftPart, chunkPart, rightPart) => leftPart + rightPart
      case _ => line
    }
    if (removed == line) {
      line
    } else {
      removeMatchedChunks(removed)
    }
  }

  private val corruptionScoreMap: Map[ClosingChar, Int] = Map(
    ")" -> 3,
    "]" -> 57,
    "}" -> 1197,
    ">" -> 25137
  )

  private val completionMap: Map[Char, ClosingChar] = Map(
    '(' -> ")",
    '[' -> "]",
    '{' -> "}",
    '<' -> ">"
  )

  private val completionScoreMap: Map[ClosingChar, Int] = Map(
    ")" -> 1,
    "]" -> 2,
    "}" -> 3,
    ">" -> 4
  )
}

type ClosingChar = "]" | "}" | ">" | ")"

case class ExpectedButWas(expected: ClosingChar, was: ClosingChar)
