package com.github.ajablonski

import com.github.ajablonski.day_11.OctopusGrid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers with AocTestData(11) {
  "part1" should "return total number of flashes in 100 steps" in {
    Day11.part1(inputFile) shouldBe 1656
  }

  "part2" should "return the moment when all octopuses flash" in {
    Day11.part2(inputFile) shouldBe 195
  }
}
