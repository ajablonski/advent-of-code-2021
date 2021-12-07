package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day6Test extends AnyFlatSpec with Matchers with AocTestData(6) {
  "part1" should "count total number of fish after 80 days" in {
    Day6.part1(inputFile) shouldBe 5934
  }

  "part2" should "count total number of fish after 256 days" in {
    Day6.part2(inputFile) shouldBe 26984457539L
  }
}
