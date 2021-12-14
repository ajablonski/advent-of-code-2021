package com.github.ajablonski

import com.github.ajablonski.day_13.{FoldInstruction, X, Y}

object Day13 extends AocProblem[Int, String] {
  override def part1(filename: String): Int = {
    val (pointSet, foldInstructions) = parse(getRawData(filename))

    applyFold(pointSet, foldInstructions.head).size
  }

  override def part2(filename: String): String = {
    val (pointSet, foldInstructions) = parse(getRawData(filename))


    val finalPoints = foldInstructions
      .foldLeft(pointSet)(applyFold)

    val maxX = finalPoints.map(_._1).max
    val maxY = finalPoints.map(_._2).max


    val result = for (y <- 0 to maxY;
                      x <- 0 to maxX) yield {
      (if (finalPoints.contains(x, y)) "#" else ".") + (if (x == maxX) "\n" else "")
    }


    result.mkString

  }

  def parse(lines: Seq[String]): (Set[(Int, Int)], Seq[FoldInstruction]) = {
    val split = lines.indexWhere(_.isEmpty)
    val (coordinatesPart, foldPart) = lines.splitAt(split)

    val xRegex = raw"fold along x=(\d+)".r
    val yRegex = raw"fold along y=(\d+)".r

    (
      coordinatesPart
        .map(line => {
          val Array(x, y) = line.split(',').map(_.toInt)
          (x, y)
        }).toSet,
      foldPart
        .tail
        .map {
          case xRegex(value) => FoldInstruction(X, value.toInt)
          case yRegex(value) => FoldInstruction(Y, value.toInt)
        }
    )
  }

  def applyFold(points: Set[(Int, Int)], foldInstruction: FoldInstruction): Set[(Int, Int)] = {
    foldInstruction match {
      case FoldInstruction(X, xEqualsValue) => points
        .map { case (x, y) => {
          if (x > xEqualsValue) {
            (-x + 2 * xEqualsValue, y)
          } else {
            (x, y)
          }
        }
        }
      case FoldInstruction(Y, yEqualsValue) => points
        .map { case (x, y) =>
          if (y > yEqualsValue) {
            (x, -y + 2 * yEqualsValue)
          } else {
            (x, y)
          }
        }
    }
  }
}
