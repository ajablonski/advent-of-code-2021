package com.github.ajablonski

import scala.annotation.tailrec
import com.github.ajablonski.day_10._


object Day10 extends AocProblem[Long, Long] {

  import com.github.ajablonski.day_10.Chunk.stringToChunkChar
  import ChunkExtensions._

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
    expectedButWas.was.getCorruptionScore
  }

  @tailrec
  def scoreCompletion(remainingSymbols: List[ClosingChar], scoreSoFar: Long = 0): Long = {
    remainingSymbols match {
      case Nil => scoreSoFar
      case (head: ClosingChar) :: rest => scoreCompletion(rest, 5 * scoreSoFar + head.getCompletionScore)
    }
  }

  @tailrec
  def completeOrError(line: List[ChunkChar],
                      expectedCharStack: List[ClosingChar] = List()): Either[ExpectedButWas, List[ClosingChar]] = {
    line match {
      case Nil => Right(expectedCharStack)
      case (head: OpeningChar) :: tail => completeOrError(tail, head.getClosingChar +: expectedCharStack)
      case (head: ClosingChar) :: tail =>
        if (expectedCharStack.head == head) {
          completeOrError(tail, expectedCharStack.tail)
        } else {
          Left(ExpectedButWas(expectedCharStack.head, head))
        }
    }
  }
}

