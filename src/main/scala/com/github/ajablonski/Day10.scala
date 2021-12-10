package com.github.ajablonski

import scala.annotation.tailrec


type ChunkChar = ClosingChar | OpeningChar
type ClosingChar = ']' | '}' | '>' | ')'
type OpeningChar = '[' | '{' | '<' | '('

object Chunk {
  given stringToChunkChar: Conversion[String, List[ChunkChar]] =
    _.toCharArray.toList.filter(_.isInstanceOf[ChunkChar]).map(_.asInstanceOf[ChunkChar])
}

case class ExpectedButWas(expected: ClosingChar, was: ClosingChar)

object Day10 extends AocProblem[Long] {
  import Chunk.stringToChunkChar

  override def part1(filename: String): Long = {
    getRawData(filename)
      .flatMap(findUnexpectedClosings)
      .map(scoreCorruption)
      .sum
  }

  override def part2(filename: String): Long = {
    val scores = getRawData(filename)
      .flatMap(line => completeOrError(line).toOption)
      .map(scoreCompletion(_))
      .sorted

    scores(scores.size / 2)
  }

  def findUnexpectedClosings(line: String): Option[ExpectedButWas] = {
    completeOrError(line)
      .left
      .toOption
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
  def completeOrError(line: List[ChunkChar],
                      expectedCharStack: List[ClosingChar] = List()): Either[ExpectedButWas, List[ClosingChar]] = {
    line match {
      case Nil => Right(expectedCharStack)
      case (head: OpeningChar) :: tail => completeOrError(tail, completionCharMap(head) +: expectedCharStack)
      case (head: ClosingChar) :: tail =>
        if (expectedCharStack.head == head) {
          completeOrError(tail, expectedCharStack.tail)
        } else {
          Left(ExpectedButWas(expectedCharStack.head, head))
        }
    }
  }

  private val corruptionScoreMap: Map[ClosingChar, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  private val completionCharMap: Map[OpeningChar, ClosingChar] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  private val completionScoreMap: Map[ClosingChar, Int] = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )
}

