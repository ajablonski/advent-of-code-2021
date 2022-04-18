package com.github.ajablonski

import com.github.ajablonski.day_18.SnailfishNumber

import scala.annotation.tailrec

object Day18 extends AocProblem[Int, Int] {
  override def part1(filename: String): Int = {
    val snfs = parseInput(getRawData(filename))

    snfs.reduce { (a, b) =>
      a + b
    }.magnitude
  }

  override def part2(filename: String): Int = {
    val snfs = parseInput(getRawData(filename))

    snfs.foldLeft(0) { (maxSoFarOuter, snfA) =>
      snfs.foldLeft(maxSoFarOuter) { (maxSoFar, snfB) =>
        math.max(maxSoFar, (snfA + snfB).magnitude)
      }
    }
  }

  def parseInput(lines: Seq[String]): Seq[SnailfishNumber] = lines
    .map(x => parseLine(x))

  @tailrec
  private def parseLine(line: String,
                        stack: List[SnailfishNumber | Int] = List()): SnailfishNumber = {
    val endBracketToken = raw"\](.*)".r
    val digitToken = raw"(\d+)(.*)".r

    line match {
      case "" => stack.head.asInstanceOf[SnailfishNumber]
      case endBracketToken(rest) => parseLine(rest, SnailfishNumber(stack.tail.head, stack.head) :: stack.tail.tail)
      case digitToken(digit, rest) => parseLine(rest, digit.toInt :: stack)
      case _ => parseLine(line.tail, stack)
    }
  }
}
