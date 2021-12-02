package com.github.ajablonski

object Day2 extends AocProblem {
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

  def parse(lines: Iterator[String]): Iterator[Movement] = {
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


case class Position(horizontal: Int, depth: Int, aim: Int = 0) {
  def advance(movement: Movement): Position = movement match {
    case Forward(change) => Position(horizontal + change, depth)
    case Down(change) => Position(horizontal, depth + change)
    case Up(change) => Position(horizontal, depth - change)
  }

  def advanceWithAim(movement: Movement): Position = movement match {
    case Forward(change) => Position(horizontal + change, depth + change * aim, aim)
    case Down(change) => Position(horizontal, depth, aim + change)
    case Up(change) => Position(horizontal, depth, aim - change)
  }
}

sealed trait Movement

case class Forward(change: Int) extends Movement

case class Down(change: Int) extends Movement

case class Up(change: Int) extends Movement