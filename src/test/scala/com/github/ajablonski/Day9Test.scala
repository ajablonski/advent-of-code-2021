package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day9Test extends AnyFlatSpec with Matchers with AocTestData(9) {
  "part1" should "return sum of risk levels of low points on the map" in {
    Day9.part1(inputFile) shouldBe 15
  }

  "part2" should "return the product of the sizes of the 3 largest basins" in {
    Day9.part2(inputFile) shouldBe 1134
  }

  "parse" should "returned indexed view of data" in {
    Day9.parse(Seq(
      "219",
      "398",
      "985"
    )) shouldBe Map(
      (0, 0) -> 2, (0, 1) -> 1, (0, 2) -> 9,
      (1, 0) -> 3, (1, 1) -> 9, (1, 2) -> 8,
      (2, 0) -> 9, (2, 1) -> 8, (2, 2) -> 5
    )
  }
}
