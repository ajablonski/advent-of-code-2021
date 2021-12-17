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
}
