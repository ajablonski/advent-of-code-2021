package com.github.ajablonski.day_2

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
