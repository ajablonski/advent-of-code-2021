package com.github.ajablonski

import com.github.ajablonski.day_2.{Down, Forward, Movement, Position, Up}

object Day2 extends AocProblem[Int, Int] {
  override def part1(filename: String): Int = {
    val finalPosition =
      parse(this.getRawData(filename))
        .foldLeft(Position(0, 0))((position, movement) => position.advance(movement))

    finalPosition.depth * finalPosition.horizontal
  }

  override def part2(filename: String): Int = {
    val finalPosition =
      parse(this.getRawData(filename))
        .foldLeft(Position(0, 0, 0))((position, movement) => position.advanceWithAim(movement))

    finalPosition.depth * finalPosition.horizontal
  }

  def parse(lines: Seq[String]): Seq[Movement] = {
    lines
      .map {
        _.split(" ") match {
          case Array("forward", change: String) => Forward(change.toInt)
          case Array("up", change: String) => Up(change.toInt)
          case Array("down", change: String) => Down(change.toInt)
        }
      }
  }
}
