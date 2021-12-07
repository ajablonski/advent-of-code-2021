package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day7Test extends AnyFlatSpec with Matchers with AocTestData(7) {
  "part1" should "return total fuel used to align" in {
    Day7.part1(inputFile) shouldBe 37
  }

  "part2" should "return total fuel used to align" in {
    Day7.part2(inputFile) shouldBe 168
  }

  "buildPositionList" should "build list of positions and counts" in {
    Day7.buildPositionList(Seq(1,3,3,2,8)) shouldBe Map(1 -> 1, 3 -> 2, 2 -> 1, 8 -> 1)
  }

  "calculateTotalCost" should "find total cost of moving to position" in {
    Day7.calculateTotalCost(Map(1 -> 1, 3 -> 2, 2 -> 1, 8 -> 1), 8) shouldBe
      (1*(1+2+3+4+5+6+7) + 2*(1+2+3+4+5) + 1*(1+2+3+4+5+6) + 0)
  }
}
