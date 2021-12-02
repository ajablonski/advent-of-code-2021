package com.github.ajablonski

import com.github.ajablonski.day_2.{Down, Forward, Position, Up}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Test extends AnyFlatSpec with Matchers {

  "part1" should "return the product of depth and horizontal position" in {
    Day2.part1(getClass.getResource("/samples/day2.txt").getFile) shouldEqual 150
  }

  "part2" should "return the product of depth and horizontal position using aim method" in {
    Day2.part2(getClass.getResource("/samples/day2.txt").getFile) shouldEqual 900
  }

  "advance" should "move forward" in {
    Position(0, 0).advance(Forward(2)) shouldBe Position(2, 0)
  }

  it should "move down" in {
    Position(0, 0).advance(Down(2)) shouldBe Position(0, 2)
  }

  it should "move up" in {
    Position(0, 10).advance(Up(2)) shouldBe Position(0, 8)
  }

  "advanceWithAim" should "move forward when aim is 0" in {
    Position(0, 0, 0).advanceWithAim(Forward(2)) shouldBe Position(2, 0, 0)
  }

  it should "move forward and down when aim is positive" in {
    Position(0, 0, 1).advanceWithAim(Forward(2)) shouldBe Position(2, 2, 1)
  }

  it should "move forward and up when aim is negative" in {
    Position(0, 0, -1).advanceWithAim(Forward(2)) shouldBe Position(2, -2, -1)
  }

  it should "move down" in {
    Position(0, 0, 0).advanceWithAim(Down(2)) shouldBe Position(0, 0, 2)
  }

  it should "move up" in {
    Position(0, 0, 0).advanceWithAim(Up(2)) shouldBe Position(0, 0, -2)
  }

  "parse" should "parse file data" in {
    Day2.parse(Seq(
      "forward 2",
      "down 10",
      "up 1"
    )) shouldEqual Seq(Forward(2), Down(10), Up(1))
  }
}
